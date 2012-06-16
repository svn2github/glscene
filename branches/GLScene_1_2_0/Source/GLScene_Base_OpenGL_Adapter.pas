// This unit is part of the GLScene Project, http://glscene.org

{ : OpenGLAdapter<p>

  <b>History : </b><font size=-1><ul>
  <li>08/09/11 - Yar - Added WGL_NV_DX_interop
  <li>21/08/11 - Yar - Added OpenGL ES
  <li>31/07/11 - Yar - Added GL_NV_Path_rendering
  <li>18/07/11 - Yar - Added WGL_EXT_create_context_es2_profile
  <li>06/06/11 - Yar - Added GL_NV_vertex_buffer_unified_memory, GL_NV_shader_buffer_load
  <li>11/03/11 - Yar - Added GL_EXT_texture_sRGB_decode, GL_ARB_separate_shader_objects, EXT_direct_state_access
  <li>19/02/11 - PREDATOR - Added Apple Extentions, Loading Apple functions
  <li>16/02/11 - PREDATOR - Added support for Mac OS X. Tested on Mac OS X 10.6.5.
  <li>18/01/11 - Yar - Added entry points for AGL
  <li>23/10/10 - Yar - Added GL_NV_vdpau_interop
  <li>25/09/10 - Yar - Added GL_get_program_binary
  <li>10/09/10 - Yar - Added GL_ATI_Meminfo, GL_NVX_gpu_memory_info
  <li>04/08/10 - Yar - Added GL_AMDX_debug_output, GL_ARB_debug_output extension. Added WGL and GLX
  <li>21/05/10 - Yar - Creation
  </ul></font>
}

unit GLScene_Base_OpenGL_Adapter;

interface

{$I GLScene.inc}
{$IFDEF DARWIN}
  {$LINKFRAMEWORK OpenGL}
  {$LINKFRAMEWORK AGL}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF }
{$IFDEF UNIX}
  Types, LCLType, dynlibs,
{$ENDIF}
{$IFDEF GLS_X11_SUPPORT}
  Xlib, X, XUtil,
{$ENDIF}
{$IFDEF DARWIN}
  MacOSAll,
{$ENDIF}
  GLScene_Base_OpenGL_Tokens,
  GLScene_Base_Vector_Geometry,
  GLScene_Base_Vector_Types,
  GLScene_Platform,
  SysUtils;

type

  Tproc=procedure;
  EOpenGLError = class(Exception);

  TGLExtensionsAndEntryPoints = class
  private
    FBuffer: string;
    FInitialized: boolean;
    FDebug: boolean;
    FDebugIds: GLuint;
    function CheckExtension(const Extension: string): boolean;
{$IFDEF SUPPORT_WGL}
    procedure ReadWGLExtensions;
    procedure ReadWGLImplementationProperties;
{$ENDIF}
{$IFDEF SUPPORT_GLX}
    procedure ReadGLXExtensions;
    procedure ReadGLXImplementationProperties;
{$ENDIF}
{$IFDEF DARWIN}
    procedure ReadAGLExtensions;
    procedure ReadAGLImplementationProperties;
{$ENDIF}
    function GetAddress(ProcName: string; glCap: Pointer): Pointer;
    function GetAddressNoSuffixes(ProcName: string; glCap: Pointer): Pointer;
    function GetAddressAlt(ProcName1, ProcName2: string; glCap: Pointer): Pointer;
  public

{$IFDEF GLS_REGIONS}{$REGION 'Extensions'}{$ENDIF}
    // supported version checks
    VERSION_1_0, VERSION_1_1, VERSION_1_2, VERSION_1_3, VERSION_1_4,
    VERSION_1_5, VERSION_2_0, VERSION_2_1, VERSION_3_0, VERSION_3_1,
    VERSION_3_2, VERSION_3_3, VERSION_4_0, VERSION_4_1, VERSION_4_2: boolean;

{$IFDEF GLS_OPENGL_ES}
   EGL_VERSION_1_0, EGL_VERSION_1_1, EGL_VERSION_1_2, EGL_VERSION_1_3,
   EGL_VERSION_1_4: Boolean;
{$ENDIF}

    // ARB approved OpenGL extension checks
    ARB_blend_func_extended, ARB_color_buffer_float, ARB_compatibility,
    ARB_copy_buffer, ARB_depth_buffer_float, ARB_depth_clamp, ARB_depth_texture,
    ARB_draw_buffers, ARB_draw_buffers_blend, ARB_draw_elements_base_vertex,
    ARB_draw_indirect, ARB_draw_instanced, ARB_explicit_attrib_location,
    ARB_fragment_coord_conventions, ARB_fragment_program, ARB_fragment_program_shadow,
    ARB_fragment_shader, ARB_framebuffer_object, ARB_framebuffer_sRGB,
    ARB_geometry_shader4, ARB_gpu_shader_fp64, ARB_gpu_shader5,
    ARB_half_float_pixel, ARB_half_float_vertex, ARB_imaging, ARB_instanced_arrays,
    ARB_map_buffer_range, ARB_matrix_palette, ARB_multisample, ARB_multitexture,
    ARB_occlusion_query, ARB_occlusion_query2, ARB_pixel_buffer_object,
    ARB_point_parameters, ARB_point_sprite, ARB_provoking_vertex,
    ARB_sample_shading, ARB_sampler_objects, ARB_seamless_cube_map,
    ARB_shader_bit_encoding, ARB_shader_subroutine, ARB_shader_texture_lod,
    ARB_shading_language_100, ARB_shadow, ARB_shadow_ambient, ARB_shader_objects,
    ARB_sync, ARB_tessellation_shader, ARB_texture_border_clamp,
    ARB_texture_buffer_object, ARB_texture_buffer_object_rgb32,
    ARB_texture_compression, ARB_texture_compression_rgtc, ARB_texture_cube_map,
    ARB_texture_cube_map_array, ARB_texture_env_add, ARB_texture_env_combine,
    ARB_texture_env_crossbar, ARB_texture_env_dot3, ARB_texture_float,
    ARB_texture_gather, ARB_texture_mirrored_repeat, ARB_texture_multisample,
    ARB_texture_non_power_of_two, ARB_texture_query_lod, ARB_texture_rectangle,
    ARB_texture_rg, ARB_texture_rgb10_a2ui, ARB_texture_swizzle,
    ARB_timer_query, ARB_transform_feedback2, ARB_transform_feedback3,
    ARB_transpose_matrix, ARB_uniform_buffer_object, ARB_vertex_array_bgra,
    ARB_vertex_array_object, ARB_vertex_blend, ARB_vertex_buffer_object,
    ARB_vertex_program, ARB_vertex_shader, ARB_vertex_type_2_10_10_10_rev,
    ARB_window_pos, ARB_texture_compression_bptc, ARB_get_program_binary,
    ARB_separate_shader_objects,

    // Vendor/EXT OpenGL extension checks
    _3DFX_multisample, _3DFX_tbuffer, _3DFX_texture_compression_FXT1,
    ATI_draw_buffers, ATI_texture_compression_3dc, ATI_texture_float,
    ATI_texture_mirror_once, S3_s3tc, EXT_abgr, EXT_bgra, EXT_bindable_uniform,
    EXT_blend_color, EXT_blend_equation_separate, EXT_blend_func_separate,
    EXT_blend_logic_op, EXT_blend_minmax, EXT_blend_subtract, EXT_Cg_shader,
    EXT_clip_volume_hint, EXT_compiled_vertex_array, EXT_copy_texture,
    EXT_depth_bounds_test, EXT_draw_buffers2, EXT_draw_instanced,
    EXT_draw_range_elements, EXT_fog_coord, EXT_framebuffer_blit,
    EXT_framebuffer_multisample, EXT_framebuffer_object, EXT_framebuffer_sRGB,
    EXT_geometry_shader4, EXT_gpu_program_parameters, EXT_gpu_shader4,
    EXT_multi_draw_arrays, EXT_multisample, EXT_packed_depth_stencil,
    EXT_packed_float, EXT_packed_pixels, EXT_paletted_texture,
    EXT_pixel_buffer_object, EXT_polygon_offset, EXT_rescale_normal,
    EXT_secondary_color, EXT_separate_specular_color, EXT_shadow_funcs,
    EXT_shared_texture_palette, EXT_stencil_clear_tag, EXT_stencil_two_side,
    EXT_stencil_wrap, EXT_texture3D, EXT_texture_array,
    EXT_texture_buffer_object, EXT_texture_compression_latc,
    EXT_texture_compression_rgtc, EXT_texture_compression_s3tc,
    EXT_texture_cube_map, EXT_texture_edge_clamp, EXT_texture_env_add,
    EXT_texture_env_combine, EXT_texture_env_dot3, EXT_texture_filter_anisotropic,
    EXT_texture_integer, EXT_texture_lod, EXT_texture_lod_bias,
    EXT_texture_mirror_clamp, EXT_texture_object, EXT_texture_rectangle,
    EXT_texture_sRGB, EXT_texture_shared_exponent, EXT_timer_query,
    EXT_transform_feedback, EXT_vertex_array, EXT_texture_sRGB_decode,
    EXT_direct_state_access, EXT_texture_swizzle, HP_occlusion_test,
    IBM_rasterpos_clip, KTX_buffer_region, MESA_resize_buffers, NV_blend_square,
    NV_conditional_render, NV_copy_image, NV_depth_buffer_float, NV_fence,
    NV_float_buffer, NV_fog_distance, NV_geometry_program4, NV_light_max_exponent,
    NV_multisample_filter_hint, NV_occlusion_query, NV_point_sprite,
    NV_primitive_restart, NV_register_combiners, NV_shader_buffer_load,
    NV_texgen_reflection, NV_texture_compression_vtc, NV_texture_env_combine4,
    NV_texture_rectangle, NV_texture_shader, NV_texture_shader2,
    NV_texture_shader3, NV_transform_feedback, NV_vertex_array_range,
    NV_vertex_array_range2, NV_vertex_buffer_unified_memory, NV_vertex_program,
    SGI_color_matrix, SGIS_generate_mipmap, SGIS_multisample,
    SGIS_texture_border_clamp, SGIS_texture_color_mask, SGIS_texture_edge_clamp,
    SGIS_texture_lod, SGIX_depth_texture, SGIX_shadow, SGIX_shadow_ambient,
    AMD_vertex_shader_tessellator, WIN_swap_hint, ATI_meminfo, NVX_gpu_memory_info,
    NV_vdpau_interop, NV_path_rendering,

    // Graphics Remedy's Extensions
    GREMEDY_frame_terminator, GREMEDY_string_marker: boolean;
    AMDX_debug_output, ARB_debug_output: Boolean;
{$IFDEF GLS_REGIONS}{$REGION 'OpenGL ES Extension'}{$ENDIF}
{$IFDEF GLS_OPENGL_ES}
    OES_depth24,
    OES_depth32,
    OES_depth_texture,
    OES_element_index_uint,
    OES_fbo_render_mipmap,
    OES_get_program_binary,
    OES_mapbuffer,
    OES_packed_depth_stencil,
    OES_rgb8_rgba8,
    OES_standard_derivatives,
    OES_texture_3D,
    OES_texture_float,
    OES_texture_float_linear,
    OES_texture_half_float,
    OES_texture_half_float_linear,
    OES_texture_npot,
    OES_vertex_array_object,
    OES_vertex_half_float,
    EXT_texture_format_BGRA8888,
    EXT_read_format_bgra,
    ANGLE_framebuffer_blit,
    EXT_texture_compression_dxt1,
    ANGLE_framebuffer_multisample : Boolean;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'OpenGL 1.1 core functions and procedures'}{$ENDIF}
    BindTexture:
    procedure(target: TGLEnum; texture: TGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    BlendFunc:
    procedure(sfactor: TGLEnum; dfactor: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Clear:
    procedure(mask: TGLbitfield);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ClearColor:
    procedure(red, green, blue, alpha: TGLclampf);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ClearDepth:
    procedure(depth: TGLclampd);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ClearStencil:
    procedure(s: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ColorMask:
    procedure(red, green, blue, alpha: TGLboolean);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    CopyTexImage1D:
    procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum;
      X, y: TGLint; Width: TGLsizei; border: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    CopyTexImage2D:
    procedure(target: TGLEnum; level: TGLint; internalFormat: TGLEnum;
      X, y: TGLint; Width, Height: TGLsizei; border: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    CopyTexSubImage1D:
    procedure(target: TGLEnum; level, xoffset, X, y: TGLint; Width: TGLsizei);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    CopyTexSubImage2D:
    procedure(target: TGLEnum; level, xoffset, yoffset, X, y: TGLint;
      Width, Height: TGLsizei);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    CullFace:
    procedure(mode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DeleteTextures:
    procedure(n: TGLsizei; textures: PGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DepthFunc:
    procedure(func: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DepthMask:
    procedure(flag: TGLboolean);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DepthRange:
    procedure(zNear, zFar: TGLclampd);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Disable:
    procedure(cap: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DrawArrays:
    procedure(mode: TGLEnum; First: TGLint; Count: TGLsizei);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DrawBuffer:
    procedure(mode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DrawElements:
    procedure(mode: TGLEnum; Count: TGLsizei; atype: TGLEnum; indices: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Enable:
    procedure(cap: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Finish:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Flush:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    FrontFace:
    procedure(mode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GenTextures:
    procedure(n: TGLsizei; textures: PGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetBooleanv:
    procedure(pname: TGLEnum; params: PGLboolean);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetDoublev:
    procedure(pname: TGLEnum; params: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetError:
    function: TGLuint;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetFloatv:
    procedure(pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetIntegerv:
    procedure(pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetPointerv:
    procedure(pname: TGLEnum; var params);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetString:
    function(Name: TGLEnum): PGLChar;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetTexImage:
    procedure(target: TGLEnum; level: TGLint; format, atype: TGLEnum; pixels: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetTexLevelParameterfv:
    procedure(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetTexLevelParameteriv:
    procedure(target: TGLEnum; level: TGLint; pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetTexParameterfv:
    procedure(target, pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetTexParameteriv:
    procedure(target, pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Hint:
    procedure(target, mode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    IsEnabled:
    function(cap: TGLEnum): TGLboolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    IsTexture:
    function(texture: TGLuint): TGLboolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    LineWidth:
    procedure(Width: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    LogicOp:
    procedure(opcode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PixelStoref:
    procedure(pname: TGLEnum; param: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PixelStorei:
    procedure(pname: TGLEnum; param: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PointSize:
    procedure(size: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PolygonMode:
    procedure(face, mode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PolygonOffset:
    procedure(factor, units: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ReadBuffer:
    procedure(mode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ReadPixels:
    procedure(X, y: TGLint; Width, Height: TGLsizei; format, atype: TGLEnum;
      pixels: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Scissor:
    procedure(X, y: TGLint; Width, Height: TGLsizei);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    StencilFunc:
    procedure(func: TGLEnum; ref: TGLint; mask: TGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    StencilMask:
    procedure(mask: TGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    StencilOp:
    procedure(fail, zfail, zpass: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexImage1D:
    procedure(target: TGLEnum; level, internalFormat: TGLint; Width: TGLsizei;
      border: TGLint; format, atype: TGLEnum; pixels: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexImage2D:
    procedure(target: TGLEnum; level, internalFormat: TGLint; Width, Height: TGLsizei;
      border: TGLint; format, atype: TGLEnum; pixels: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexParameterf:
    procedure(target, pname: TGLEnum; param: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexParameterfv:
    procedure(target, pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexParameteri:
    procedure(target, pname: TGLEnum; param: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexParameteriv:
    procedure(target, pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexSubImage1D:
    procedure(target: TGLEnum; level, xoffset: TGLint; Width: TGLsizei;
      format, atype: TGLEnum; pixels: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexSubImage2D:
    procedure(target: TGLEnum; level, xoffset, yoffset: TGLint;
      Width, Height: TGLsizei; format, atype: TGLEnum; pixels: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Viewport:
    procedure(X, y: TGLint; Width, Height: TGLsizei);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'OpenGL 1.1 deprecated'}{$ENDIF}
    Accum:
    procedure(op: TGLuint; Value: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    AlphaFunc:
    procedure(func: TGLEnum; ref: TGLclampf);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    AreTexturesResident:
    function(n: TGLsizei; textures: PGLuint; residences: PGLboolean): TGLboolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ArrayElement:
    procedure(i: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Begin_:
    procedure(mode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Bitmap:
    procedure(Width: TGLsizei; Height: TGLsizei; xorig, yorig: TGLfloat;
      xmove: TGLfloat; ymove: TGLfloat; Bitmap: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    CallList:
    procedure(list: TGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    CallLists:
    procedure(n: TGLsizei; atype: TGLEnum; lists: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ClearAccum:
    procedure(red, green, blue, alpha: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ClearIndex:
    procedure(c: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ClipPlane:
    procedure(plane: TGLEnum; equation: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3b:
    procedure(red, green, blue: TGLbyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3bv:
    procedure(v: PGLbyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3d:
    procedure(red, green, blue: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3f:
    procedure(red, green, blue: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3i:
    procedure(red, green, blue: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3s:
    procedure(red, green, blue: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3ub:
    procedure(red, green, blue: TGLubyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3ubv:
    procedure(v: PGLubyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3ui:
    procedure(red, green, blue: TGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3uiv:
    procedure(v: PGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3us:
    procedure(red, green, blue: TGLushort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color3usv:
    procedure(v: PGLushort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4b:
    procedure(red, green, blue, alpha: TGLbyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4bv:
    procedure(v: PGLbyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4d:
    procedure(red, green, blue, alpha: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4f:
    procedure(red, green, blue, alpha: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4i:
    procedure(red, green, blue, alpha: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4s:
    procedure(red, green, blue, alpha: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4sv:
    procedure(v: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4ub:
    procedure(red, green, blue, alpha: TGLubyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4ubv:
    procedure(v: PGLubyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4ui:
    procedure(red, green, blue, alpha: TGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4uiv:
    procedure(v: PGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4us:
    procedure(red, green, blue, alpha: TGLushort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Color4usv:
    procedure(v: PGLushort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ColorMaterial:
    procedure(face: TGLEnum; mode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ColorPointer:
    procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei; Data: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    CopyPixels:
    procedure(X, y: TGLint; Width, Height: TGLsizei; atype: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DeleteLists:
    procedure(list: TGLuint; range: TGLsizei);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DisableClientState:
    procedure(aarray: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DrawPixels:
    procedure(Width, Height: TGLsizei; format, atype: TGLEnum; pixels: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EdgeFlag:
    procedure(flag: TGLboolean);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EdgeFlagPointer:
    procedure(stride: TGLsizei; Data: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EdgeFlagv:
    procedure(flag: PGLboolean);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EnableClientState:
    procedure(aarray: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    End_:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EndList:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalCoord1d:
    procedure(u: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalCoord1dv:
    procedure(u: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalCoord1f:
    procedure(u: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalCoord1fv:
    procedure(u: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalCoord2d:
    procedure(u: TGLdouble; v: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalCoord2dv:
    procedure(u: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalCoord2f:
    procedure(u, v: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalCoord2fv:
    procedure(u: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalMesh1:
    procedure(mode: TGLEnum; i1, i2: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalMesh2:
    procedure(mode: TGLEnum; i1, i2, j1, j2: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalPoint1:
    procedure(i: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EvalPoint2:
    procedure(i, j: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    FeedbackBuffer:
    procedure(size: TGLsizei; atype: TGLEnum; buffer: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Fogf:
    procedure(pname: TGLEnum; param: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Fogfv:
    procedure(pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Fogi:
    procedure(pname: TGLEnum; param: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Fogiv:
    procedure(pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Frustum:
    procedure(left, right, bottom, top, zNear, zFar: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GenLists:
    function(range: TGLsizei): TGLuint;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetClipPlane:
    procedure(plane: TGLEnum; equation: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetLightfv:
    procedure(light, pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetLightiv:
    procedure(light, pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetMapdv:
    procedure(target, query: TGLEnum; v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetMapfv:
    procedure(target, query: TGLEnum; v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetMapiv:
    procedure(target, query: TGLEnum; v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetMaterialfv:
    procedure(face, pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetMaterialiv:
    procedure(face, pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetPixelMapfv:
    procedure(map: TGLEnum; values: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetPixelMapuiv:
    procedure(map: TGLEnum; values: PGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetPixelMapusv:
    procedure(map: TGLEnum; values: PGLushort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetPolygonStipple:
    procedure(mask: PGLubyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetTexEnvfv:
    procedure(target, pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetTexEnviv:
    procedure(target, pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetTexGendv:
    procedure(coord, pname: TGLEnum; params: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetTexGenfv:
    procedure(coord, pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetTexGeniv:
    procedure(coord, pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    IndexMask:
    procedure(mask: TGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    IndexPointer:
    procedure(atype: TGLEnum; stride: TGLsizei; Data: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Indexd:
    procedure(c: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Indexdv:
    procedure(c: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Indexf:
    procedure(c: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Indexfv:
    procedure(c: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Indexi:
    procedure(c: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Indexiv:
    procedure(c: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Indexs:
    procedure(c: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Indexsv:
    procedure(c: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Indexub:
    procedure(c: TGLubyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Indexubv:
    procedure(c: PGLubyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    InitNames:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    InterleavedArrays:
    procedure(format: TGLEnum; stride: TGLsizei; Data: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    IsList:
    function(list: TGLuint): TGLboolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    LightModelf:
    procedure(pname: TGLEnum; param: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    LightModelfv:
    procedure(pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    LightModeli:
    procedure(pname: TGLEnum; param: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    LightModeliv:
    procedure(pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Lightf:
    procedure(light, pname: TGLEnum; param: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Lightfv:
    procedure(light, pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Lighti:
    procedure(light, pname: TGLEnum; param: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Lightiv:
    procedure(light, pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    LineStipple:
    procedure(factor: TGLint; pattern: TGLushort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ListBase:
    procedure(base: TGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    LoadIdentity:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    LoadMatrixd:
    procedure(m: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    LoadMatrixf:
    procedure(m: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    LoadName:
    procedure(Name: TGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Map1d:
    procedure(target: TGLEnum; u1, u2: TGLdouble; stride, order: TGLint; points: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Map1f:
    procedure(target: TGLEnum; u1, u2: TGLfloat; stride, order: TGLint; points: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Map2d:
    procedure(target: TGLEnum; u1, u2: TGLdouble; ustride, uorder: TGLint;
      v1, v2: TGLdouble; vstride, vorder: TGLint; points: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Map2f:
    procedure(target: TGLEnum; u1, u2: TGLfloat; ustride, uorder: TGLint;
      v1, v2: TGLfloat; vstride, vorder: TGLint; points: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    MapGrid1d:
    procedure(un: TGLint; u1, u2: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    MapGrid1f:
    procedure(un: TGLint; u1, u2: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    MapGrid2d:
    procedure(un: TGLint; u1, u2: TGLdouble; vn: TGLint; v1, v2: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    MapGrid2f:
    procedure(un: TGLint; u1, u2: TGLfloat; vn: TGLint; v1, v2: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Materialf:
    procedure(face, pname: TGLEnum; param: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Materialfv:
    procedure(face, pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Materiali:
    procedure(face, pname: TGLEnum; param: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Materialiv:
    procedure(face, pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    MatrixMode:
    procedure(mode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    MultMatrixd:
    procedure(m: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    MultMatrixf:
    procedure(m: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    NewList:
    procedure(list: TGLuint; mode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Normal3b:
    procedure(nx, ny, nz: TGLbyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Normal3bv:
    procedure(v: PGLbyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Normal3d:
    procedure(nx, ny, nz: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Normal3dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Normal3f:
    procedure(nx, ny, nz: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Normal3fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Normal3i:
    procedure(nx, ny, nz: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Normal3iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Normal3s:
    procedure(nx, ny, nz: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Normal3sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    NormalPointer:
    procedure(atype: TGLEnum; stride: TGLsizei; Data: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Ortho:
    procedure(left, right, bottom, top, zNear, zFar: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PassThrough:
    procedure(token: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PixelMapfv:
    procedure(map: TGLEnum; mapsize: TGLsizei; values: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PixelMapuiv:
    procedure(map: TGLEnum; mapsize: TGLsizei; values: PGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PixelMapusv:
    procedure(map: TGLEnum; mapsize: TGLsizei; values: PGLushort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PixelTransferf:
    procedure(pname: TGLEnum; param: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PixelTransferi:
    procedure(pname: TGLEnum; param: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PixelZoom:
    procedure(xfactor, yfactor: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PolygonStipple:
    procedure(mask: PGLubyte);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PopAttrib:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PopClientAttrib:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PopMatrix:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PopName:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PrioritizeTextures:
    procedure(n: TGLsizei; textures: PGLuint; priorities: PGLclampf);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PushAttrib:
    procedure(mask: TGLbitfield);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PushClientAttrib:
    procedure(mask: TGLbitfield);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PushMatrix:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    PushName:
    procedure(Name: TGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos2d:
    procedure(X, y: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos2dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos2f:
    procedure(X, y: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos2fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos2i:
    procedure(X, y: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos2iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos2s:
    procedure(X, y: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos2sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos3d:
    procedure(X, y, z: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos3dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos3f:
    procedure(X, y, z: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos3fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos3i:
    procedure(X, y, z: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos3iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos3s:
    procedure(X, y, z: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos3sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos4d:
    procedure(X, y, z, w: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos4dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos4f:
    procedure(X, y, z, w: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos4fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos4i:
    procedure(X, y, z, w: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos4iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos4s:
    procedure(X, y, z, w: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RasterPos4sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Rectd:
    procedure(x1, y1, x2, y2: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Rectdv:
    procedure(v1, v2: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Rectf:
    procedure(x1, y1, x2, y2: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Rectfv:
    procedure(v1, v2: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Recti:
    procedure(x1, y1, x2, y2: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Rectiv:
    procedure(v1, v2: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Rects:
    procedure(x1, y1, x2, y2: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Rectsv:
    procedure(v1, v2: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    RenderMode:
    function(mode: TGLEnum): TGLint;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Rotated:
    procedure(ane, X, y, z: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Rotatef:
    procedure(ane, X, y, z: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Scaled:
    procedure(X, y, z: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Scalef:
    procedure(X, y, z: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    SelectBuffer:
    procedure(size: TGLsizei; buffer: PGLuint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ShadeModel:
    procedure(mode: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord1d:
    procedure(s: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord1dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord1f:
    procedure(s: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord1fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord1i:
    procedure(s: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord1iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord1s:
    procedure(s: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord1sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord2d:
    procedure(s, t: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord2dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord2f:
    procedure(s, t: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord2fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord2i:
    procedure(s, t: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord2iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord2s:
    procedure(s, t: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord2sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord3d:
    procedure(s, t, r: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord3dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord3f:
    procedure(s, t, r: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord3fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord3i:
    procedure(s, t, r: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord3iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord3s:
    procedure(s, t, r: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord3sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord4d:
    procedure(s, t, r, q: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord4dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord4f:
    procedure(s, t, r, q: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord4fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord4i:
    procedure(s, t, r, q: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord4iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord4s:
    procedure(s, t, r, q: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoord4sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexCoordPointer:
    procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei; Data: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexEnvf:
    procedure(target, pname: TGLEnum; param: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexEnvfv:
    procedure(target, pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexEnvi:
    procedure(target, pname: TGLEnum; param: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexEnviv:
    procedure(target, pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexGend:
    procedure(coord, pname: TGLEnum; param: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexGendv:
    procedure(coord, pname: TGLEnum; params: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexGenf:
    procedure(coord, pname: TGLEnum; param: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexGenfv:
    procedure(coord, pname: TGLEnum; params: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexGeni:
    procedure(coord, pname: TGLEnum; param: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    TexGeniv:
    procedure(coord, pname: TGLEnum; params: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Translated:
    procedure(X, y, z: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Translatef:
    procedure(X, y, z: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex2d:
    procedure(X, y: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex2dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex2f:
    procedure(X, y: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex2fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex2i:
    procedure(X, y: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex2iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex2s:
    procedure(X, y: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex2sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex3d:
    procedure(X, y, z: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex3dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex3f:
    procedure(X, y, z: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex3fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex3i:
    procedure(X, y, z: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex3iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex3s:
    procedure(X, y, z: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex3sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex4d:
    procedure(X, y, z, w: TGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex4dv:
    procedure(v: PGLdouble);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex4f:
    procedure(X, y, z, w: TGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex4fv:
    procedure(v: PGLfloat);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex4i:
    procedure(X, y, z, w: TGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex4iv:
    procedure(v: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex4s:
    procedure(X, y, z, w: TGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    Vertex4sv:
    procedure(v: PGLshort);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    VertexPointer:
    procedure(size: TGLint; atype: TGLEnum; stride: TGLsizei; Data: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'New core function/procedure definitions in OpenGL 1.2'}
{$ENDIF}
    BlendColor: PFNGLBLENDCOLORPROC;
    BlendEquation: PFNGLBLENDEQUATIONPROC;
    DrawRangeElements: PFNGLDRAWRANGEELEMENTSPROC;
    TexImage3D: PFNGLTEXIMAGE3DPROC;
    TexSubImage3D: PFNGLTEXSUBIMAGE3DPROC;
    CopyTexSubImage3D: PFNGLCOPYTEXSUBIMAGE3DPROC;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'New core function/procedure definitions in OpenGL 1.4'}
{$ENDIF}
    BlendFuncSeparate: PFNGLBLENDFUNCSEPARATEPROC;
    MultiDrawArrays: PFNGLMULTIDRAWARRAYSPROC;
    MultiDrawElements: PFNGLMULTIDRAWELEMENTSPROC;
    PointParameterf: PFNGLPOINTPARAMETERFPROC;
    PointParameterfv: PFNGLPOINTPARAMETERFVPROC;
    PointParameteri: PFNGLPOINTPARAMETERIPROC;
    PointParameteriv: PFNGLPOINTPARAMETERIVPROC;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'New core function/procedure definitions in OpenGL 2.0'}
{$ENDIF}
    BlendEquationSeparate: PFNGLBLENDEQUATIONSEPARATEPROC;
    DrawBuffers: PFNGLDRAWBUFFERSPROC;
    StencilOpSeparate: PFNGLSTENCILOPSEPARATEPROC;
    StencilFuncSeparate: PFNGLSTENCILFUNCSEPARATEPROC;
    StencilMaskSeparate: PFNGLSTENCILMASKSEPARATEPROC;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Vertex buffer object'}{$ENDIF}
    LockArrays: PFNGLLOCKARRAYSEXTPROC; // EXT only
    UnlockArrays: PFNGLUNLOCKARRAYSEXTPROC; // EXT only
    BindBuffer: PFNGLBINDBUFFERPROC;
    DeleteBuffers: PFNGLDELETEBUFFERSPROC;
    GenBuffers: PFNGLGENBUFFERSPROC;
    IsBuffer: PFNGLISBUFFERPROC;
    BufferData: PFNGLBUFFERDATAPROC;
    BufferSubData: PFNGLBUFFERSUBDATAPROC;
    GetBufferSubData: PFNGLGETBUFFERSUBDATAPROC;
    MapBuffer: PFNGLMAPBUFFERPROC;
    UnmapBuffer: PFNGLUNMAPBUFFERPROC;
    GetBufferParameteriv: PFNGLGETBUFFERPARAMETERIVPROC;
    GetBufferPointerv: PFNGLGETBUFFERPOINTERVPROC;
    MapBufferRange: PFNGLMAPBUFFERRANGEPROC;
    FlushMappedBufferRange: PFNGLFLUSHMAPPEDBUFFERRANGEPROC;
    BindBufferRange: PFNGLBINDBUFFERRANGEPROC;
    BindBufferOffset: PFNGLBINDBUFFEROFFSETEXTPROC; // EXT + NV only
    BindBufferBase: PFNGLBINDBUFFERBASEPROC;
    TransformFeedbackAttribs: PFNGLTRANSFORMFEEDBACKATTRIBSNVPROC; // NV only
    TransformFeedbackVaryingsNV: PFNGLTRANSFORMFEEDBACKVARYINGSNVPROC; // NV only
    TransformFeedbackVaryings: PFNGLTRANSFORMFEEDBACKVARYINGSPROC;
    GetTransformFeedbackVarying: PFNGLGETTRANSFORMFEEDBACKVARYINGPROC;
    BeginTransformFeedback: PFNGLBEGINTRANSFORMFEEDBACKPROC;
    EndTransformFeedback: PFNGLENDTRANSFORMFEEDBACKPROC;
    TexBuffer: PFNGLTEXBUFFERPROC;
    ClearBufferiv: PFNGLCLEARBUFFERIVPROC;
    ClearBufferuiv: PFNGLCLEARBUFFERUIVPROC;
    ClearBufferfv: PFNGLCLEARBUFFERFVPROC;
    ClearBufferfi: PFNGLCLEARBUFFERFIPROC;
    GetStringi: PFNGLGETSTRINGIPROC;
    BindVertexArray: PFNGLBINDVERTEXARRAYPROC;
    DeleteVertexArrays: PFNGLDELETEVERTEXARRAYSPROC;
    GenVertexArrays: PFNGLGENVERTEXARRAYSPROC;
    IsVertexArray: PFNGLISVERTEXARRAYPROC;
    FlushVertexArrayRangeNV: PFNGLFLUSHVERTEXARRAYRANGENVPROC;
    VertexArrayRangeNV: PFNGLVERTEXARRAYRANGENVPROC;
    GetUniformIndices: PFNGLGETUNIFORMINDICESPROC;
    GetActiveUniformsiv: PFNGLGETACTIVEUNIFORMSIVPROC;
    GetActiveUniformName: PFNGLGETACTIVEUNIFORMNAMEPROC;
    GetUniformBlockIndex: PFNGLGETUNIFORMBLOCKINDEXPROC;
    GetActiveUniformBlockiv: PFNGLGETACTIVEUNIFORMBLOCKIVPROC;
    GetActiveUniformBlockName: PFNGLGETACTIVEUNIFORMBLOCKNAMEPROC;
    UniformBlockBinding: PFNGLUNIFORMBLOCKBINDINGPROC;
    CopyBufferSubData: PFNGLCOPYBUFFERSUBDATAPROC;
    UniformBuffer: PFNGLUNIFORMBUFFEREXTPROC;
    GetUniformBufferSize: PFNGLGETUNIFORMBUFFERSIZEEXTPROC; // EXT only
    GetUniformOffset: PFNGLGETUNIFORMOFFSETEXTPROC; // EXT only
    PrimitiveRestartIndex: PFNGLPRIMITIVERESTARTINDEXPROC;
    DrawElementsBaseVertex: PFNGLDRAWELEMENTSBASEVERTEXPROC;
    DrawRangeElementsBaseVertex: PFNGLDRAWRANGEELEMENTSBASEVERTEXPROC;
    DrawElementsInstancedBaseVertex: PFNGLDRAWELEMENTSINSTANCEDBASEVERTEXPROC;
    MultiDrawElementsBaseVertex: PFNGLMULTIDRAWELEMENTSBASEVERTEXPROC;
    DrawArraysInstanced: PFNGLDRAWARRAYSINSTANCEDPROC;
    DrawElementsInstanced: PFNGLDRAWELEMENTSINSTANCEDPROC;
    VertexAttrib1d: PFNGLVERTEXATTRIB1DPROC;
    VertexAttrib1dv: PFNGLVERTEXATTRIB1DVPROC;
    VertexAttrib1f: PFNGLVERTEXATTRIB1FPROC;
    VertexAttrib1fv: PFNGLVERTEXATTRIB1FVPROC;
    VertexAttrib1s: PFNGLVERTEXATTRIB1SPROC;
    VertexAttrib1sv: PFNGLVERTEXATTRIB1SVPROC;
    VertexAttrib2d: PFNGLVERTEXATTRIB2DPROC;
    VertexAttrib2dv: PFNGLVERTEXATTRIB2DVPROC;
    VertexAttrib2f: PFNGLVERTEXATTRIB2FPROC;
    VertexAttrib2fv: PFNGLVERTEXATTRIB2FVPROC;
    VertexAttrib2s: PFNGLVERTEXATTRIB2SPROC;
    VertexAttrib2sv: PFNGLVERTEXATTRIB2SVPROC;
    VertexAttrib3d: PFNGLVERTEXATTRIB3DPROC;
    VertexAttrib3dv: PFNGLVERTEXATTRIB3DVPROC;
    VertexAttrib3f: PFNGLVERTEXATTRIB3FPROC;
    VertexAttrib3fv: PFNGLVERTEXATTRIB3FVPROC;
    VertexAttrib3s: PFNGLVERTEXATTRIB3SPROC;
    VertexAttrib3sv: PFNGLVERTEXATTRIB3SVPROC;
    VertexAttrib4Nbv: PFNGLVERTEXATTRIB4NBVPROC;
    VertexAttrib4Niv: PFNGLVERTEXATTRIB4NIVPROC;
    VertexAttrib4Nsv: PFNGLVERTEXATTRIB4NSVPROC;
    VertexAttrib4Nub: PFNGLVERTEXATTRIB4NUBPROC;
    VertexAttrib4Nubv: PFNGLVERTEXATTRIB4NUBVPROC;
    VertexAttrib4Nuiv: PFNGLVERTEXATTRIB4NUIVPROC;
    VertexAttrib4Nusv: PFNGLVERTEXATTRIB4NUSVPROC;
    VertexAttrib4bv: PFNGLVERTEXATTRIB4BVPROC;
    VertexAttrib4d: PFNGLVERTEXATTRIB4DPROC;
    VertexAttrib4dv: PFNGLVERTEXATTRIB4DVPROC;
    VertexAttrib4f: PFNGLVERTEXATTRIB4FPROC;
    VertexAttrib4fv: PFNGLVERTEXATTRIB4FVPROC;
    VertexAttrib4iv: PFNGLVERTEXATTRIB4IVPROC;
    VertexAttrib4s: PFNGLVERTEXATTRIB4SPROC;
    VertexAttrib4sv: PFNGLVERTEXATTRIB4SVPROC;
    VertexAttrib4ubv: PFNGLVERTEXATTRIB4UBVPROC;
    VertexAttrib4uiv: PFNGLVERTEXATTRIB4UIVPROC;
    VertexAttrib4usv: PFNGLVERTEXATTRIB4USVPROC;
    VertexAttribPointer: PFNGLVERTEXATTRIBPOINTERPROC;
    VertexAttribI1i: PFNGLVERTEXATTRIBI1IPROC;
    VertexAttribI2i: PFNGLVERTEXATTRIBI2IPROC;
    VertexAttribI3i: PFNGLVERTEXATTRIBI3IPROC;
    VertexAttribI4i: PFNGLVERTEXATTRIBI4IPROC;
    VertexAttribI1ui: PFNGLVERTEXATTRIBI1UIPROC;
    VertexAttribI2ui: PFNGLVERTEXATTRIBI2UIPROC;
    VertexAttribI3ui: PFNGLVERTEXATTRIBI3UIPROC;
    VertexAttribI4ui: PFNGLVERTEXATTRIBI4UIPROC;
    VertexAttribI1iv: PFNGLVERTEXATTRIBI1IVPROC;
    VertexAttribI2iv: PFNGLVERTEXATTRIBI2IVPROC;
    VertexAttribI3iv: PFNGLVERTEXATTRIBI3IVPROC;
    VertexAttribI4iv: PFNGLVERTEXATTRIBI4IVPROC;
    VertexAttribI1uiv: PFNGLVERTEXATTRIBI1UIVPROC;
    VertexAttribI2uiv: PFNGLVERTEXATTRIBI2UIVPROC;
    VertexAttribI3uiv: PFNGLVERTEXATTRIBI3UIVPROC;
    VertexAttribI4uiv: PFNGLVERTEXATTRIBI4UIVPROC;
    VertexAttribI4bv: PFNGLVERTEXATTRIBI4BVPROC;
    VertexAttribI4sv: PFNGLVERTEXATTRIBI4SVPROC;
    VertexAttribI4ubv: PFNGLVERTEXATTRIBI4UBVPROC;
    VertexAttribI4usv: PFNGLVERTEXATTRIBI4USVPROC;
    VertexAttribIPointer: PFNGLVERTEXATTRIBIPOINTERPROC;
    GetVertexAttribIiv: PFNGLGETVERTEXATTRIBIIVPROC;
    GetVertexAttribIuiv: PFNGLGETVERTEXATTRIBIUIVPROC;
    Uniform1ui: PFNGLUNIFORM1UIPROC;
    Uniform2ui: PFNGLUNIFORM2UIPROC;
    Uniform3ui: PFNGLUNIFORM3UIPROC;
    Uniform4ui: PFNGLUNIFORM4UIPROC;
    Uniform1uiv: PFNGLUNIFORM1UIVPROC;
    Uniform2uiv: PFNGLUNIFORM2UIVPROC;
    Uniform3uiv: PFNGLUNIFORM3UIVPROC;
    Uniform4uiv: PFNGLUNIFORM4UIVPROC;
    GetUniformuiv: PFNGLGETUNIFORMUIVPROC;
    BindFragDataLocation: PFNGLBINDFRAGDATALOCATIONPROC;
    GetFragDataLocation: PFNGLGETFRAGDATALOCATIONPROC;
    ClampColor: PFNGLCLAMPCOLORPROC;
    ColorMaski: PFNGLCOLORMASKIPROC;
    GetBooleani_v: PFNGLGETBOOLEANI_VPROC;
    GetIntegeri_v: PFNGLGETINTEGERI_VPROC;
    Enablei: PFNGLENABLEIPROC;
    Disablei: PFNGLDISABLEIPROC;
    IsEnabledi: PFNGLISENABLEDIPROC;
    EnableVertexAttribArray: PFNGLENABLEVERTEXATTRIBARRAYPROC;
    DisableVertexAttribArray: PFNGLDISABLEVERTEXATTRIBARRAYPROC;
    VertexAttribDivisor: PFNGLVERTEXATTRIBDIVISORPROC;
    ClearColorIi: PFNGLCLEARCOLORIIEXTPROC; // EXT only
    ClearColorIui: PFNGLCLEARCOLORIUIEXTPROC; // EXT only
    TexParameterIiv: PFNGLTEXPARAMETERIIVPROC;
    TexParameterIuiv: PFNGLTEXPARAMETERIUIVPROC;
    GetTexParameterIiv: PFNGLGETTEXPARAMETERIIVPROC;
    GetTexParameterIuiv: PFNGLGETTEXPARAMETERIUIVPROC;
    PatchParameteri: PFNGLPATCHPARAMETERIPROC;
    PatchParameterfv: PFNGLPATCHPARAMETERFVPROC;
    BufferAddressRangeNV: PFNGLBUFFERADDRESSRANGENVPROC;
    VertexFormatNV: PFNGLVERTEXFORMATNVPROC;
    NormalFormatNV: PFNGLNORMALFORMATNVPROC;
    ColorFormatNV: PFNGLCOLORFORMATNVPROC;
    IndexFormatNV: PFNGLINDEXFORMATNVPROC;
    TexCoordFormatNV: PFNGLTEXCOORDFORMATNVPROC;
    EdgeFlagFormatNV: PFNGLEDGEFLAGFORMATNVPROC;
    SecondaryColorFormatNV: PFNGLSECONDARYCOLORFORMATNVPROC;
    FogCoordFormatNV: PFNGLFOGCOORDFORMATNVPROC;
    VertexAttribFormatNV: PFNGLVERTEXATTRIBFORMATNVPROC;
    VertexAttribIFormatNV: PFNGLVERTEXATTRIBIFORMATNVPROC;
    GetIntegerui64i_vNV: PFNGLGETINTEGERUI64I_VNVPROC;
    GetBufferParameterui64vNV: PGNGLGETBUFFERPARAMETERUI64VNV;
    MakeBufferResidentNV: PFNGLMAKEBUFFERRESIDENTNVPROC;
    MakeBufferNonResidentNV: PFNGLMAKEBUFFERNONRESIDENTNVPROC;
    IsBufferResidentNV: PFNGLISBUFFERRESIDENTNVPROC;
    MakeNamedBufferResidentNV: PFNGLMAKENAMEDBUFFERRESIDENTNVPROC;
    MakeNamedBufferNonResidentNV: PFNGLMAKENAMEDBUFFERNONRESIDENTNVPROC;
    IsNamedBufferResidentNV: PFNGLISNAMEDBUFFERRESIDENTNVPROC;
    GetNamedBufferParameterui64vNV: PFNGLGETNAMEDBUFFERPARAMETERUI64VNVPROC;
    GetIntegerui64vNV: PFNGLGETINTEGERUI64VNVPROC;
    Uniformui64NV: PFNGLUNIFORMUI64NVPROC;
    Uniformui64vNV: PFNGLUNIFORMUI64VNVPROC;
    GetUniformui64vNV: PFNGLGETUNIFORMUI64VNVPROC;
    ProgramUniformui64NV: PFNGLPROGRAMUNIFORMUI64NVPROC;
    ProgramUniformui64vNV: PFNGLPROGRAMUNIFORMUI64VNVPROC;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Shader object'}{$ENDIF}
    DeleteObject: PFNGLDELETEOBJECTARBPROC; // ARB only
{$IFDEF GLS_OPENGL_ES}
    DeleteShader: PFNGLDELETEOBJECTARBPROC;
    DeleteProgram: PFNGLDELETEOBJECTARBPROC;
{$ENDIF}
    GetHandle: PFNGLGETHANDLEARBPROC; // ARB only
    DetachShader: PFNGLDETACHSHADERPROC;
    CreateShader: PFNGLCREATESHADERPROC;
    ShaderSource: PFNGLSHADERSOURCEPROC;
    CompileShader: PFNGLCOMPILESHADERPROC;
    CreateProgram: PFNGLCREATEPROGRAMPROC;
    AttachShader: PFNGLATTACHSHADERPROC;
    LinkProgram: PFNGLLINKPROGRAMPROC;
    UseProgram: PFNGLUSEPROGRAMPROC;
    ValidateProgram: PFNGLVALIDATEPROGRAMPROC;
    Uniform1f: PFNGLUNIFORM1FPROC;
    Uniform2f: PFNGLUNIFORM2FPROC;
    Uniform3f: PFNGLUNIFORM3FPROC;
    Uniform4f: PFNGLUNIFORM4FPROC;
    Uniform1i: PFNGLUNIFORM1IPROC;
    Uniform2i: PFNGLUNIFORM2IPROC;
    Uniform3i: PFNGLUNIFORM3IPROC;
    Uniform4i: PFNGLUNIFORM4IPROC;
    Uniform1fv: PFNGLUNIFORM1FVPROC;
    Uniform2fv: PFNGLUNIFORM2FVPROC;
    Uniform3fv: PFNGLUNIFORM3FVPROC;
    Uniform4fv: PFNGLUNIFORM4FVPROC;
    Uniform1iv: PFNGLUNIFORM1IVPROC;
    Uniform2iv: PFNGLUNIFORM2IVPROC;
    Uniform3iv: PFNGLUNIFORM3IVPROC;
    Uniform4iv: PFNGLUNIFORM4IVPROC;
    UniformMatrix2fv: PFNGLUNIFORMMATRIX2FVPROC;
    UniformMatrix3fv: PFNGLUNIFORMMATRIX3FVPROC;
    UniformMatrix4fv: PFNGLUNIFORMMATRIX4FVPROC;
    GetObjectParameterfv: PFNGLGETOBJECTPARAMETERFVARBPROC; // ARB only
    GetObjectParameteriv: PFNGLGETOBJECTPARAMETERIVARBPROC; // ARB only
    GetInfoLog: PFNGLGETINFOLOGARBPROC; // ARB only
    GetAttachedObjects: PFNGLGETATTACHEDOBJECTSARBPROC; // ARB only
    GetActiveAttrib: PFNGLGETACTIVEATTRIBPROC;
    GetActiveUniform: PFNGLGETACTIVEUNIFORMPROC;
    GetAttachedShaders: PFNGLGETATTACHEDSHADERSPROC;
    GetAttribLocation: PFNGLGETATTRIBLOCATIONPROC;
    GetProgramiv: PFNGLGETPROGRAMIVPROC;
    GetProgramInfoLog: PFNGLGETPROGRAMINFOLOGPROC;
    GetShaderiv: PFNGLGETSHADERIVPROC;
    GetShaderInfoLog: PFNGLGETSHADERINFOLOGPROC;
    GetShaderSource: PFNGLGETSHADERSOURCEPROC;
    GetUniformLocation: PFNGLGETUNIFORMLOCATIONPROC;
    GetUniformfv: PFNGLGETUNIFORMFVPROC;
    GetUniformiv: PFNGLGETUNIFORMIVPROC;
    GetVertexAttribdv: PFNGLGETVERTEXATTRIBDVPROC;
    GetVertexAttribfv: PFNGLGETVERTEXATTRIBFVPROC;
    GetVertexAttribiv: PFNGLGETVERTEXATTRIBIVPROC;
    GetVertexAttribPointerv: PFNGLGETVERTEXATTRIBPOINTERVPROC;
    IsProgram: PFNGLISPROGRAMPROC;
    IsShader: PFNGLISSHADERPROC;
    BindAttribLocation: PFNGLBINDATTRIBLOCATIONPROC;
    BindFragDataLocationIndexed: PFNGLBINDFRAGDATALOCATIONINDEXEDPROC;
    GetFragDataIndex: PFNGLGETFRAGDATAINDEXPROC;
    GetVaryingLocation: PFNGLGETVARYINGLOCATIONNVPROC; // NV only
    GetActiveVarying: PFNGLGETACTIVEVARYINGNVPROC; // NV only
    ActiveVarying: PFNGLACTIVEVARYINGNVPROC; // NV only
    GetProgramBinary: PFNGLGETPROGRAMBINARYPROC;
    ProgramBinary: PFNGLPROGRAMBINARYPROC;
    UseProgramStages: PFNGLUSEPROGRAMSTAGESPROC;
    ActiveShaderProgram: PFNGLACTIVESHADERPROGRAMPROC;
    CreateShaderProgramv: PFNGLCREATESHADERPROGRAMVPROC;
    BindProgramPipeline: PFNGLBINDPROGRAMPIPELINEPROC;
    DeleteProgramPipelines: PFNGLDELETEPROGRAMPIPELINESPROC;
    GenProgramPipelines: PFNGLGENPROGRAMPIPELINESPROC;
    IsProgramPipeline: PFNGLISPROGRAMPIPELINEPROC;
    GetProgramPipelineiv: PFNGLGETPROGRAMPIPELINEIVPROC;
    ProgramUniform1i: PFNGLPROGRAMUNIFORM1IPROC;
    ProgramUniform1iv: PFNGLPROGRAMUNIFORM1IVPROC;
    ProgramUniform1f: PFNGLPROGRAMUNIFORM1FPROC;
    ProgramUniform1fv: PFNGLPROGRAMUNIFORM1FVPROC;
    ProgramUniform1d: PFNGLPROGRAMUNIFORM1DPROC;
    ProgramUniform1dv: PFNGLPROGRAMUNIFORM1DVPROC;
    ProgramUniform1ui: PFNGLPROGRAMUNIFORM1UIPROC;
    ProgramUniform1uiv: PFNGLPROGRAMUNIFORM1UIVPROC;
    ProgramUniform2i: PFNGLPROGRAMUNIFORM2IPROC;
    ProgramUniform2iv: PFNGLPROGRAMUNIFORM2IVPROC;
    ProgramUniform2f: PFNGLPROGRAMUNIFORM2FPROC;
    ProgramUniform2fv: PFNGLPROGRAMUNIFORM2FVPROC;
    ProgramUniform2d: PFNGLPROGRAMUNIFORM2DPROC;
    ProgramUniform2dv: PFNGLPROGRAMUNIFORM2DVPROC;
    ProgramUniform2ui: PFNGLPROGRAMUNIFORM2UIPROC;
    ProgramUniform2uiv: PFNGLPROGRAMUNIFORM2UIVPROC;
    ProgramUniform3i: PFNGLPROGRAMUNIFORM3IPROC;
    ProgramUniform3iv: PFNGLPROGRAMUNIFORM3IVPROC;
    ProgramUniform3f: PFNGLPROGRAMUNIFORM3FPROC;
    ProgramUniform3fv: PFNGLPROGRAMUNIFORM3FVPROC;
    ProgramUniform3d: PFNGLPROGRAMUNIFORM3DPROC;
    ProgramUniform3dv: PFNGLPROGRAMUNIFORM3DVPROC;
    ProgramUniform3ui: PFNGLPROGRAMUNIFORM3UIPROC;
    ProgramUniform3uiv: PFNGLPROGRAMUNIFORM3UIVPROC;
    ProgramUniform4i: PFNGLPROGRAMUNIFORM4IPROC;
    ProgramUniform4iv: PFNGLPROGRAMUNIFORM4IVPROC;
    ProgramUniform4f: PFNGLPROGRAMUNIFORM4FPROC;
    ProgramUniform4fv: PFNGLPROGRAMUNIFORM4FVPROC;
    ProgramUniform4d: PFNGLPROGRAMUNIFORM4DPROC;
    ProgramUniform4dv: PFNGLPROGRAMUNIFORM4DVPROC;
    ProgramUniform4ui: PFNGLPROGRAMUNIFORM4UIPROC;
    ProgramUniform4uiv: PFNGLPROGRAMUNIFORM4UIVPROC;
    ProgramUniformMatrix2fv: PFNGLPROGRAMUNIFORMMATRIX2FVPROC;
    ProgramUniformMatrix3fv: PFNGLPROGRAMUNIFORMMATRIX3FVPROC;
    ProgramUniformMatrix4fv: PFNGLPROGRAMUNIFORMMATRIX4FVPROC;
    ProgramUniformMatrix2dv: PFNGLPROGRAMUNIFORMMATRIX2DVPROC;
    ProgramUniformMatrix3dv: PFNGLPROGRAMUNIFORMMATRIX3DVPROC;
    ProgramUniformMatrix4dv: PFNGLPROGRAMUNIFORMMATRIX4DVPROC;
    ProgramUniformMatrix2x3fv: PFNGLPROGRAMUNIFORMMATRIX2X3FVPROC;
    ProgramUniformMatrix3x2fv: PFNGLPROGRAMUNIFORMMATRIX3X2FVPROC;
    ProgramUniformMatrix2x4fv: PFNGLPROGRAMUNIFORMMATRIX2X4FVPROC;
    ProgramUniformMatrix4x2fv: PFNGLPROGRAMUNIFORMMATRIX4X2FVPROC;
    ProgramUniformMatrix3x4fv: PFNGLPROGRAMUNIFORMMATRIX3X4FVPROC;
    ProgramUniformMatrix4x3fv: PFNGLPROGRAMUNIFORMMATRIX4X3FVPROC;
    ProgramUniformMatrix2x3dv: PFNGLPROGRAMUNIFORMMATRIX2X3DVPROC;
    ProgramUniformMatrix3x2dv: PFNGLPROGRAMUNIFORMMATRIX3X2DVPROC;
    ProgramUniformMatrix2x4dv: PFNGLPROGRAMUNIFORMMATRIX2X4DVPROC;
    ProgramUniformMatrix4x2dv: PFNGLPROGRAMUNIFORMMATRIX4X2DVPROC;
    ProgramUniformMatrix3x4dv: PFNGLPROGRAMUNIFORMMATRIX3X4DVPROC;
    ProgramUniformMatrix4x3dv: PFNGLPROGRAMUNIFORMMATRIX4X3DVPROC;
    ValidateProgramPipeline: PFNGLVALIDATEPROGRAMPIPELINEPROC;
    GetProgramPipelineInfoLog: PFNGLGETPROGRAMPIPELINEINFOLOGPROC;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Framebuffer object'}{$ENDIF}
    IsRenderbuffer: PFNGLISRENDERBUFFERPROC;
    BindRenderbuffer: PFNGLBINDRENDERBUFFERPROC;
    DeleteRenderbuffers: PFNGLDELETERENDERBUFFERSPROC;
    GenRenderbuffers: PFNGLGENRENDERBUFFERSPROC;
    RenderbufferStorage: PFNGLRENDERBUFFERSTORAGEPROC;
    RenderbufferStorageMultisample: PFNGLRENDERBUFFERSTORAGEMULTISAMPLEPROC;
    GetRenderbufferParameteriv: PFNGLGETRENDERBUFFERPARAMETERIVPROC;
    IsFramebuffer: PFNGLISFRAMEBUFFERPROC;
    BindFramebuffer: PFNGLBINDFRAMEBUFFERPROC;
    DeleteFramebuffers: PFNGLDELETEFRAMEBUFFERSPROC;
    GenFramebuffers: PFNGLGENFRAMEBUFFERSPROC;
    CheckFramebufferStatus: PFNGLCHECKFRAMEBUFFERSTATUSPROC;
    FramebufferTexture: PFNGLFRAMEBUFFERTEXTUREPROC;
    FramebufferTexture1D: PFNGLFRAMEBUFFERTEXTURE1DPROC;
    FramebufferTexture2D: PFNGLFRAMEBUFFERTEXTURE2DPROC;
    FramebufferTexture3D: PFNGLFRAMEBUFFERTEXTURE3DPROC;
    FramebufferTextureLayer: PFNGLFRAMEBUFFERTEXTURELAYERPROC;
    FramebufferTextureFace: PFNGLFRAMEBUFFERTEXTUREFACEARBPROC; // ARB only
    FramebufferRenderbuffer: PFNGLFRAMEBUFFERRENDERBUFFERPROC;
    GetFramebufferAttachmentParameteriv: PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVPROC;
    BlitFramebuffer: PFNGLBLITFRAMEBUFFERPROC;
    GenerateMipmap: PFNGLGENERATEMIPMAPPROC;
    DepthBounds: PFNGLDEPTHBOUNDSEXTPROC;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Queries object'}{$ENDIF}
    GenQueries: PFNGLGENQUERIESPROC;
    DeleteQueries: PFNGLDELETEQUERIESPROC;
    IsQuery: PFNGLISQUERYPROC;
    BeginQuery: PFNGLBEGINQUERYPROC;
    EndQuery: PFNGLENDQUERYPROC;
    GetQueryiv: PFNGLGETQUERYIVPROC;
    GetQueryObjectiv: PFNGLGETQUERYOBJECTIVPROC;
    GetQueryObjectuiv: PFNGLGETQUERYOBJECTUIVPROC;
    QueryCounter: PFNGLQUERYCOUNTERPROC;
    GetQueryObjecti64v: PFNGLGETQUERYOBJECTI64VPROC;
    GetQueryObjectui64v: PFNGLGETQUERYOBJECTUI64VPROC;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Sampler object'}{$ENDIF}
    // promoted to core v1.3 from GL_ARB_multitexture (#1)
    ActiveTexture: PFNGLACTIVETEXTUREPROC;
    SampleCoverage: PFNGLSAMPLECOVERAGEPROC;
    // promoted to core v1.3 from GL_ARB_texture_compression (#12)
    CompressedTexImage3D: PFNGLCOMPRESSEDTEXIMAGE3DPROC;
    CompressedTexImage2D: PFNGLCOMPRESSEDTEXIMAGE2DPROC;
    CompressedTexImage1D: PFNGLCOMPRESSEDTEXIMAGE1DPROC;
    CompressedTexSubImage3D: PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC;
    CompressedTexSubImage2D: PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC;
    CompressedTexSubImage1D: PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC;
    GetCompressedTexImage: PFNGLGETCOMPRESSEDTEXIMAGEPROC;
    ClientActiveTexture: PFNGLCLIENTACTIVETEXTUREPROC;
    MultiTexCoord1d: PFNGLMULTITEXCOORD1DPROC;
    MultiTexCoord1dV: PFNGLMULTITEXCOORD1DVPROC;
    MultiTexCoord1f: PFNGLMULTITEXCOORD1FPROC;
    MultiTexCoord1fv: PFNGLMULTITEXCOORD1FVPROC;
    MultiTexCoord1i: PFNGLMULTITEXCOORD1IPROC;
    MultiTexCoord1iv: PFNGLMULTITEXCOORD1IVPROC;
    MultiTexCoord1s: PFNGLMULTITEXCOORD1SPROC;
    MultiTexCoord1sv: PFNGLMULTITEXCOORD1SVPROC;
    MultiTexCoord2d: PFNGLMULTITEXCOORD2DPROC;
    MultiTexCoord2dv: PFNGLMULTITEXCOORD2DVPROC;
    MultiTexCoord2f: PFNGLMULTITEXCOORD2FPROC;
    MultiTexCoord2fv: PFNGLMULTITEXCOORD2FVPROC;
    MultiTexCoord2i: PFNGLMULTITEXCOORD2IPROC;
    MultiTexCoord2iv: PFNGLMULTITEXCOORD2IVPROC;
    MultiTexCoord2s: PFNGLMULTITEXCOORD2SPROC;
    MultiTexCoord2sv: PFNGLMULTITEXCOORD2SVPROC;
    MultiTexCoord3d: PFNGLMULTITEXCOORD3DPROC;
    MultiTexCoord3dv: PFNGLMULTITEXCOORD3DVPROC;
    MultiTexCoord3f: PFNGLMULTITEXCOORD3FPROC;
    MultiTexCoord3fv: PFNGLMULTITEXCOORD3FVPROC;
    MultiTexCoord3i: PFNGLMULTITEXCOORD3IPROC;
    MultiTexCoord3iv: PFNGLMULTITEXCOORD3IVPROC;
    MultiTexCoord3s: PFNGLMULTITEXCOORD3SPROC;
    MultiTexCoord3sv: PFNGLMULTITEXCOORD3SVPROC;
    MultiTexCoord4d: PFNGLMULTITEXCOORD4DPROC;
    MultiTexCoord4dv: PFNGLMULTITEXCOORD4DVPROC;
    MultiTexCoord4f: PFNGLMULTITEXCOORD4FPROC;
    MultiTexCoord4fv: PFNGLMULTITEXCOORD4FVPROC;
    MultiTexCoord4i: PFNGLMULTITEXCOORD4IPROC;
    MultiTexCoord4iv: PFNGLMULTITEXCOORD4IVPROC;
    MultiTexCoord4s: PFNGLMULTITEXCOORD4SPROC;
    MultiTexCoord4sv: PFNGLMULTITEXCOORD4SVPROC;
    GetInteger64i_v: PFNGLGETINTEGER64I_VPROC;
    GetBufferParameteri64v: PFNGLGETBUFFERPARAMETERI64VPROC;
    ProgramParameteri: PFNGLPROGRAMPARAMETERIPROC;
    ProgramString: PFNGLPROGRAMSTRINGARBPROC; // ARB only
    BindProgram: PFNGLBINDPROGRAMARBPROC; // ARB + NV only
    DeletePrograms: PFNGLDELETEPROGRAMSARBPROC; // ARB + NV only
    GenPrograms: PFNGLGENPROGRAMSARBPROC; // ARB + NV only
    ProgramEnvParameter4d: PFNGLPROGRAMENVPARAMETER4DARBPROC; // ARB only
    ProgramEnvParameter4dv: PFNGLPROGRAMENVPARAMETER4DVARBPROC; // ARB only
    ProgramEnvParameter4f: PFNGLPROGRAMENVPARAMETER4FARBPROC; // ARB only
    ProgramEnvParameter4fv: PFNGLPROGRAMENVPARAMETER4FVARBPROC; // ARB only
    ProgramLocalParameter4d: PFNGLPROGRAMLOCALPARAMETER4DARBPROC; // ARB only
    ProgramLocalParameter4dv: PFNGLPROGRAMLOCALPARAMETER4DVARBPROC; // ARB only
    ProgramLocalParameter4f: PFNGLPROGRAMLOCALPARAMETER4FARBPROC; // ARB only
    ProgramLocalParameter4fv: PFNGLPROGRAMLOCALPARAMETER4FVARBPROC; // ARB only
    GetProgramEnvParameterdv: PFNGLGETPROGRAMENVPARAMETERDVARBPROC; // ARB only
    GetProgramEnvParameterfv: PFNGLGETPROGRAMENVPARAMETERFVARBPROC; // ARB only
    GetProgramLocalParameterdv: PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC; // ARB only
    GetProgramLocalParameterfv: PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC; // ARB only
    TexImage2DMultisample: PFNGLTEXIMAGE2DMULTISAMPLEPROC;
    TexImage3DMultisample: PFNGLTEXIMAGE3DMULTISAMPLEPROC;
    GetMultisamplefv: PFNGLGETMULTISAMPLEFVPROC;
    SampleMaski: PFNGLSAMPLEMASKIPROC;
    ProvokingVertex: PFNGLPROVOKINGVERTEXPROC;
    FenceSync: PFNGLFENCESYNCPROC;
    IsSync: PFNGLISSYNCPROC;
    DeleteSync: PFNGLDELETESYNCPROC;
    ClientWaitSync: PFNGLCLIENTWAITSYNCPROC;
    WaitSync: PFNGLWAITSYNCPROC;
    GetInteger64v: PFNGLGETINTEGER64VPROC;
    GetSynciv: PFNGLGETSYNCIVPROC;
    BlendEquationi: PFNGLBLENDEQUATIONIPROC;
    BlendEquationSeparatei: PFNGLBLENDEQUATIONSEPARATEIPROC;
    BlendFunci: PFNGLBLENDFUNCIPROC;
    BlendFuncSeparatei: PFNGLBLENDFUNCSEPARATEIPROC;
    MinSampleShading: PFNGLMINSAMPLESHADINGPROC;
    GenSamplers: PFNGLGENSAMPLERSPROC;
    DeleteSamplers: PFNGLDELETESAMPLERSPROC;
    IsSampler: PFNGLISSAMPLERPROC;
    BindSampler: PFNGLBINDSAMPLERPROC;
    SamplerParameteri: PFNGLSAMPLERPARAMETERIPROC;
    SamplerParameteriv: PFNGLSAMPLERPARAMETERIVPROC;
    SamplerParameterf: PFNGLSAMPLERPARAMETERFPROC;
    SamplerParameterfv: PFNGLSAMPLERPARAMETERFVPROC;
    SamplerParameterIiv: PFNGLSAMPLERPARAMETERIIVPROC;
    SamplerParameterIuiv: PFNGLSAMPLERPARAMETERIUIVPROC;
    GetSamplerParameteriv: PFNGLGETSAMPLERPARAMETERIVPROC;
    GetSamplerParameterIiv: PFNGLGETSAMPLERPARAMETERIIVPROC;
    GetSamplerParameterfv: PFNGLGETSAMPLERPARAMETERFVPROC;
    GetSamplerParameterIfv: PFNGLGETSAMPLERPARAMETERIFVPROC;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Direct access'}{$ENDIF}
    ClientAttribDefault: PFNGLCLIENTATTRIBDEFAULTEXTPROC;
    PushClientAttribDefault: PFNGLPUSHCLIENTATTRIBDEFAULTEXTPROC;
    MatrixLoadf: PFNGLMATRIXLOADFEXTPROC;
    MatrixLoadd: PFNGLMATRIXLOADDEXTPROC;
    MatrixMultf: PFNGLMATRIXMULTFEXTPROC;
    MatrixMultd: PFNGLMATRIXMULTDEXTPROC;
    MatrixLoadIdentity: PFNGLMATRIXLOADIDENTITYEXTPROC;
    MatrixRotatef: PFNGLMATRIXROTATEFEXTPROC;
    MatrixRotated: PFNGLMATRIXROTATEDEXTPROC;
    MatrixScalef: PFNGLMATRIXSCALEFEXTPROC;
    MatrixScaled: PFNGLMATRIXSCALEDEXTPROC;
    MatrixTranslatef: PFNGLMATRIXTRANSLATEFEXTPROC;
    MatrixTranslated: PFNGLMATRIXTRANSLATEDEXTPROC;
    MatrixFrustum: PFNGLMATRIXFRUSTUMEXTPROC;
    MatrixOrtho: PFNGLMATRIXORTHOEXTPROC;
    MatrixPop: PFNGLMATRIXPOPEXTPROC;
    MatrixPush: PFNGLMATRIXPUSHEXTPROC;
    MatrixLoadTransposef: PFNGLMATRIXLOADTRANSPOSEFEXTPROC;
    MatrixLoadTransposed: PFNGLMATRIXLOADTRANSPOSEDEXTPROC;
    MatrixMultTransposef: PFNGLMATRIXMULTTRANSPOSEFEXTPROC;
    MatrixMultTransposed: PFNGLMATRIXMULTTRANSPOSEDEXTPROC;
    TextureParameterf: PFNGLTEXTUREPARAMETERFEXTPROC;
    TextureParameterfv: PFNGLTEXTUREPARAMETERFVEXTPROC;
    TextureParameteri: PFNGLTEXTUREPARAMETERIEXTPROC;
    TextureParameteriv: PFNGLTEXTUREPARAMETERIVEXTPROC;
    TextureImage1D: PFNGLTEXTUREIMAGE1DEXTPROC;
    TextureImage2D: PFNGLTEXTUREIMAGE2DEXTPROC;
    TextureSubImage1D: PFNGLTEXTURESUBIMAGE1DEXTPROC;
    TextureSubImage2D: PFNGLTEXTURESUBIMAGE2DEXTPROC;
    CopyTextureImage1D: PFNGLCOPYTEXTUREIMAGE1DEXTPROC;
    CopyTextureImage2D: PFNGLCOPYTEXTUREIMAGE2DEXTPROC;
    CopyTextureSubImage1D: PFNGLCOPYTEXTURESUBIMAGE1DEXTPROC;
    CopyTextureSubImage2D: PFNGLCOPYTEXTURESUBIMAGE2DEXTPROC;
    GetTextureImage: PFNGLGETTEXTUREIMAGEEXTPROC;
    GetTextureParameterfv: PFNGLGETTEXTUREPARAMETERFVEXTPROC;
    GetTextureParameteriv: PFNGLGETTEXTUREPARAMETERIVEXTPROC;
    GetTextureLevelParameterfv: PFNGLGETTEXTURELEVELPARAMETERFVEXTPROC;
    GetTextureLevelParameteriv: PFNGLGETTEXTURELEVELPARAMETERIVEXTPROC;
    TextureImage3D: PFNGLTEXTUREIMAGE3DEXTPROC;
    TextureSubImage3D: PFNGLTEXTURESUBIMAGE3DEXTPROC;
    CopyTextureSubImage3D: PFNGLCOPYTEXTURESUBIMAGE3DEXTPROC;
    MultiTexParameterf: PFNGLMULTITEXPARAMETERFEXTPROC;
    MultiTexParameterfv: PFNGLMULTITEXPARAMETERFVEXTPROC;
    MultiTexParameteri: PFNGLMULTITEXPARAMETERIEXTPROC;
    MultiTexParameteriv: PFNGLMULTITEXPARAMETERIVEXTPROC;
    MultiTexImage1D: PFNGLMULTITEXIMAGE1DEXTPROC;
    MultiTexImage2D: PFNGLMULTITEXIMAGE2DEXTPROC;
    MultiTexSubImage1D: PFNGLMULTITEXSUBIMAGE1DEXTPROC;
    MultiTexSubImage2D: PFNGLMULTITEXSUBIMAGE2DEXTPROC;
    CopyMultiTexImage1D: PFNGLCOPYMULTITEXIMAGE1DEXTPROC;
    CopyMultiTexImage2D: PFNGLCOPYMULTITEXIMAGE2DEXTPROC;
    CopyMultiTexSubImage1D: PFNGLCOPYMULTITEXSUBIMAGE1DEXTPROC;
    CopyMultiTexSubImage2D: PFNGLCOPYMULTITEXSUBIMAGE2DEXTPROC;
    GetMultiTexImage: PFNGLGETMULTITEXIMAGEEXTPROC;
    GetMultiTexParameterfv: PFNGLGETMULTITEXPARAMETERFVEXTPROC;
    GetMultiTexParameteriv: PFNGLGETMULTITEXPARAMETERIVEXTPROC;
    GetMultiTexLevelParameterfv: PFNGLGETMULTITEXLEVELPARAMETERFVEXTPROC;
    GetMultiTexLevelParameteriv: PFNGLGETMULTITEXLEVELPARAMETERIVEXTPROC;
    MultiTexImage3D: PFNGLMULTITEXIMAGE3DEXTPROC;
    MultiTexSubImage3D: PFNGLMULTITEXSUBIMAGE3DEXTPROC;
    CopyMultiTexSubImage3D: PFNGLCOPYMULTITEXSUBIMAGE3DEXTPROC;
    BindMultiTexture: PFNGLBINDMULTITEXTUREEXTPROC;
    EnableClientStateIndexed: PFNGLENABLECLIENTSTATEINDEXEDEXTPROC;
    DisableClientStateIndexed: PFNGLDISABLECLIENTSTATEINDEXEDEXTPROC;
    MultiTexCoordPointer: PFNGLMULTITEXCOORDPOINTEREXTPROC;
    MultiTexEnvf: PFNGLMULTITEXENVFEXTPROC;
    MultiTexEnvfv: PFNGLMULTITEXENVFVEXTPROC;
    MultiTexEnvi: PFNGLMULTITEXENVIEXTPROC;
    MultiTexEnviv: PFNGLMULTITEXENVIVEXTPROC;
    MultiTexGend: PFNGLMULTITEXGENDEXTPROC;
    MultiTexGendv: PFNGLMULTITEXGENDVEXTPROC;
    MultiTexGenf: PFNGLMULTITEXGENFEXTPROC;
    MultiTexGenfv: PFNGLMULTITEXGENFVEXTPROC;
    MultiTexGeni: PFNGLMULTITEXGENIEXTPROC;
    MultiTexGeniv: PFNGLMULTITEXGENIVEXTPROC;
    GetMultiTexEnvfv: PFNGLGETMULTITEXENVFVEXTPROC;
    GetMultiTexEnviv: PFNGLGETMULTITEXENVIVEXTPROC;
    GetMultiTexGendv: PFNGLGETMULTITEXGENDVEXTPROC;
    GetMultiTexGenfv: PFNGLGETMULTITEXGENFVEXTPROC;
    GetMultiTexGeniv: PFNGLGETMULTITEXGENIVEXTPROC;
    GetFloatIndexedv: PFNGLGETFLOATINDEXEDVEXTPROC;
    GetDoubleIndexedv: PFNGLGETDOUBLEINDEXEDVEXTPROC;
    GetPointerIndexedv: PFNGLGETPOINTERINDEXEDVEXTPROC;
    CompressedTextureImage3D: PFNGLCOMPRESSEDTEXTUREIMAGE3DEXTPROC;
    CompressedTextureImage2D: PFNGLCOMPRESSEDTEXTUREIMAGE2DEXTPROC;
    CompressedTextureImage1D: PFNGLCOMPRESSEDTEXTUREIMAGE1DEXTPROC;
    CompressedTextureSubImage3D: PFNGLCOMPRESSEDTEXTURESUBIMAGE3DEXTPROC;
    CompressedTextureSubImage2D: PFNGLCOMPRESSEDTEXTURESUBIMAGE2DEXTPROC;
    CompressedTextureSubImage1D: PFNGLCOMPRESSEDTEXTURESUBIMAGE1DEXTPROC;
    GetCompressedTextureImage: PFNGLGETCOMPRESSEDTEXTUREIMAGEEXTPROC;
    CompressedMultiTexImage3D: PFNGLCOMPRESSEDMULTITEXIMAGE3DEXTPROC;
    CompressedMultiTexImage2D: PFNGLCOMPRESSEDMULTITEXIMAGE2DEXTPROC;
    CompressedMultiTexImage1D: PFNGLCOMPRESSEDMULTITEXIMAGE1DEXTPROC;
    CompressedMultiTexSubImage3D: PFNGLCOMPRESSEDMULTITEXSUBIMAGE3DEXTPROC;
    CompressedMultiTexSubImage2D: PFNGLCOMPRESSEDMULTITEXSUBIMAGE2DEXTPROC;
    CompressedMultiTexSubImage1D: PFNGLCOMPRESSEDMULTITEXSUBIMAGE1DEXTPROC;
    GetCompressedMultiTexImage: PFNGLGETCOMPRESSEDMULTITEXIMAGEEXTPROC;
    NamedProgramString: PFNGLNAMEDPROGRAMSTRINGEXTPROC;
    NamedProgramLocalParameter4d: PFNGLNAMEDPROGRAMLOCALPARAMETER4DEXTPROC;
    NamedProgramLocalParameter4dv: PFNGLNAMEDPROGRAMLOCALPARAMETER4DVEXTPROC;
    NamedProgramLocalParameter4f: PFNGLNAMEDPROGRAMLOCALPARAMETER4FEXTPROC;
    NamedProgramLocalParameter4fv: PFNGLNAMEDPROGRAMLOCALPARAMETER4FVEXTPROC;
    GetNamedProgramLocalParameterdv: PFNGLGETNAMEDPROGRAMLOCALPARAMETERDVEXTPROC;
    GetNamedProgramLocalParameterfv: PFNGLGETNAMEDPROGRAMLOCALPARAMETERFVEXTPROC;
    GetNamedProgramiv: PFNGLGETNAMEDPROGRAMIVEXTPROC;
    GetNamedProgramString: PFNGLGETNAMEDPROGRAMSTRINGEXTPROC;
    NamedProgramLocalParameters4fv: PFNGLNAMEDPROGRAMLOCALPARAMETERS4FVEXTPROC;
    NamedProgramLocalParameterI4i: PFNGLNAMEDPROGRAMLOCALPARAMETERI4IEXTPROC;
    NamedProgramLocalParameterI4iv: PFNGLNAMEDPROGRAMLOCALPARAMETERI4IVEXTPROC;
    NamedProgramLocalParametersI4iv: PFNGLNAMEDPROGRAMLOCALPARAMETERSI4IVEXTPROC;
    NamedProgramLocalParameterI4ui: PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIEXTPROC;
    NamedProgramLocalParameterI4uiv: PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIVEXTPROC;
    NamedProgramLocalParametersI4uiv: PFNGLNAMEDPROGRAMLOCALPARAMETERSI4UIVEXTPROC;
    GetNamedProgramLocalParameterIiv: PFNGLGETNAMEDPROGRAMLOCALPARAMETERIIVEXTPROC;
    GetNamedProgramLocalParameterIuiv: PFNGLGETNAMEDPROGRAMLOCALPARAMETERIUIVEXTPROC;
    TextureParameterIiv: PFNGLTEXTUREPARAMETERIIVEXTPROC;
    TextureParameterIuiv: PFNGLTEXTUREPARAMETERIUIVEXTPROC;
    GetTextureParameterIiv: PFNGLGETTEXTUREPARAMETERIIVEXTPROC;
    GetTextureParameterIuiv: PFNGLGETTEXTUREPARAMETERIUIVEXTPROC;
    MultiTexParameterIiv: PFNGLMULTITEXPARAMETERIIVEXTPROC;
    MultiTexParameterIuiv: PFNGLMULTITEXPARAMETERIUIVEXTPROC;
    GetMultiTexParameterIiv: PFNGLGETMULTITEXPARAMETERIIVEXTPROC;
    GetMultiTexParameterIuiv: PFNGLGETMULTITEXPARAMETERIUIVEXTPROC;
    NamedBufferData: PFNGLNAMEDBUFFERDATAEXTPROC;
    NamedBufferSubData: PFNGLNAMEDBUFFERSUBDATAEXTPROC;
    MapNamedBuffer: PFNGLMAPNAMEDBUFFEREXTPROC;
    UnmapNamedBuffer: PFNGLUNMAPNAMEDBUFFEREXTPROC;
    MapNamedBufferRange: PFNGLMAPNAMEDBUFFERRANGEEXTPROC;
    FlushMappedNamedBufferRange: PFNGLFLUSHMAPPEDNAMEDBUFFERRANGEEXTPROC;
    NamedCopyBufferSubData: PFNGLNAMEDCOPYBUFFERSUBDATAEXTPROC;
    GetNamedBufferParameteriv: PFNGLGETNAMEDBUFFERPARAMETERIVEXTPROC;
    GetNamedBufferPointerv: PFNGLGETNAMEDBUFFERPOINTERVEXTPROC;
    GetNamedBufferSubData: PFNGLGETNAMEDBUFFERSUBDATAEXTPROC;
    TextureBuffer: PFNGLTEXTUREBUFFEREXTPROC;
    MultiTexBuffer: PFNGLMULTITEXBUFFEREXTPROC;
    NamedRenderbufferStorage: PFNGLNAMEDRENDERBUFFERSTORAGEEXTPROC;
    GetNamedRenderbufferParameteriv: PFNGLGETNAMEDRENDERBUFFERPARAMETERIVEXTPROC;
    CheckNamedFramebufferStatus: PFNGLCHECKNAMEDFRAMEBUFFERSTATUSEXTPROC;
    NamedFramebufferTexture1D: PFNGLNAMEDFRAMEBUFFERTEXTURE1DEXTPROC;
    NamedFramebufferTexture2D: PFNGLNAMEDFRAMEBUFFERTEXTURE2DEXTPROC;
    NamedFramebufferTexture3D: PFNGLNAMEDFRAMEBUFFERTEXTURE3DEXTPROC;
    NamedFramebufferRenderbuffer: PFNGLNAMEDFRAMEBUFFERRENDERBUFFEREXTPROC;
    GetNamedFramebufferAttachmentParameteriv: PFNGLGETNAMEDFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC;
    GenerateTextureMipmap: PFNGLGENERATETEXTUREMIPMAPEXTPROC;
    GenerateMultiTexMipmap: PFNGLGENERATEMULTITEXMIPMAPEXTPROC;
    FramebufferDrawBuffer: PFNGLFRAMEBUFFERDRAWBUFFEREXTPROC;
    FramebufferDrawBuffers: PFNGLFRAMEBUFFERDRAWBUFFERSEXTPROC;
    FramebufferReadBuffer: PFNGLFRAMEBUFFERREADBUFFEREXTPROC;
    GetFramebufferParameteriv: PFNGLGETFRAMEBUFFERPARAMETERIVEXTPROC;
    NamedRenderbufferStorageMultisample: PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC;
    NamedRenderbufferStorageMultisampleCoverage: PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLECOVERAGEEXTPROC;
    NamedFramebufferTexture: PFNGLNAMEDFRAMEBUFFERTEXTUREEXTPROC;
    NamedFramebufferTextureLayer: PFNGLNAMEDFRAMEBUFFERTEXTURELAYEREXTPROC;
    NamedFramebufferTextureFace: PFNGLNAMEDFRAMEBUFFERTEXTUREFACEEXTPROC;
    TextureRenderbuffer: PFNGLTEXTURERENDERBUFFEREXTPROC;
    MultiTexRenderbuffer: PFNGLMULTITEXRENDERBUFFEREXTPROC;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Debugging'}{$ENDIF}
    // Special Gremedy debugger extension
    FrameTerminatorGREMEDY: PFNGLFRAMETERMINATORGREMEDYPROC;
    StringMarkerGREMEDY: PFNGLSTRINGMARKERGREMEDYPROC;
    DebugMessageEnableAMDX:
    procedure(category: GLenum; severity: GLenum; Count: GLSizei;
      var ids: GLuint; Enabled: boolean);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DebugMessageCallbackAMDX:
    procedure(callback: TDebugProcAMD; userParam: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DebugMessageControl:
    procedure(type_: GLenum; Source: GLenum; severity: GLenum; Count: GLSizei;
      var ids: GLuint; Enabled: boolean);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DebugMessageInsert:
    procedure(Source: GLenum; severity: GLenum; id: GLuint; length: GLSizei;
      const buf: PGLChar);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    DebugMessageCallback:
    procedure(callback: TDebugProc; userParam: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    GetDebugMessageLog:
    function(Count: GLuint; bufSize: GLSizei; var severity: GLenum;
      var severities: GLuint; var ids: GLuint; var lengths: GLSizei;
      messageLog: PGLChar): GLuint;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Interrop'}{$ENDIF}
{$IFDEF LINUX}
    VDPAUInitNV:
    procedure(const vdpDevice: Pointer; const getProcAddress: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    VDPAUFiniNV:
    procedure();
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    VDPAURegisterVideoSurfaceNV:
    function(const vdpSurface: Pointer; target: TGLEnum; numTextureNames: TGLsizei;
      const textureNames: PGLuint): TGLvdpauSurfaceNV;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    VDPAURegisterOutputSurfaceNV:
    function(const vdpSurface: Pointer; target: TGLEnum; numTextureNames: TGLsizei;
      const textureNames: PGLuint): TGLvdpauSurfaceNV;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    VDPAUIsSurfaceNV:
    procedure(surface: TGLvdpauSurfaceNV);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    VDPAUUnregisterSurfaceNV:
    procedure(surface: TGLvdpauSurfaceNV);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    VDPAUGetSurfaceivNV:
    procedure(surface: TGLvdpauSurfaceNV; pname: TGLEnum; bufSize: TGLsizei;
      length: PGLsizei; values: PGLint);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    VDPAUSurfaceAccessNV:
    procedure(surface: TGLvdpauSurfaceNV; access: TGLEnum);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    VDPAUMapSurfacesNV:
    procedure(numSurfaces: TGLsizei; const surfaces: PGLvdpauSurfaceNV);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    VDPAUUnmapSurfacesNV:
    procedure(numSurface: TGLsizei; const surfaces: PGLvdpauSurfaceNV);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
{$ENDIF LINUX}

{$IFDEF GLS_REGIONS}{$ENDREGION 'Interrop'}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Path rendering'}{$ENDIF}
   GenPathsNV: PFNGLGENPATHSNVPROC;
   DeletePathsNV: PFNGLDELETEPATHSNVPROC;
   IsPathNV: PFNGLISPATHNVPROC;
   PathCommandsNV: PFNGLPATHCOMMANDSNVPROC;
   PathCoordsNV: PFNGLPATHCOORDSNVPROC;
   PathSubCommandsNV: PFNGLPATHSUBCOMMANDSNVPROC;
   PathSubCoordsNV: PFNGLPATHSUBCOORDSNVPROC;
   PathStringNV: PFNGLPATHSTRINGNVPROC;
   PathGlyphsNV: PFNGLPATHGLYPHSNVPROC;
   PathGlyphRangeNV: PFNGLPATHGLYPHRANGENVPROC;
   WeightPathsNV: PFNGLWEIGHTPATHSNVPROC;
   CopyPathNV: PFNGLCOPYPATHNVPROC;
   InterpolatePathsNV: PFNGLINTERPOLATEPATHSNVPROC;
   PathParameterivNV: PFNGLPATHPARAMETERIVNVPROC;
   PathParameteriNV: PFNGLPATHPARAMETERINVPROC;
   PathParameterfvNV: PFNGLPATHPARAMETERFVNVPROC;
   PathParameterfNV: PFNGLPATHPARAMETERFNVPROC;
   PathDashArrayNV: PFNGLPATHDASHARRAYNVPROC;
   PathStencilFuncNV: PFNGLPATHSTENCILFUNCNVPROC;
   StencilFillPathNV: PFNGLSTENCILFILLPATHNVPROC;
   StencilStrokePathNV: PFNGLSTENCILSTROKEPATHNVPROC;
   StencilFillPathInstancedNV: PFNGLSTENCILFILLPATHINSTANCEDNVPROC;
   StencilStrokePathInstancedNV: PFNGLSTENCILSTROKEPATHINSTANCEDNVPROC;
   PathColorGenNV: PFNGLPATHCOLORGENNVPROC;
   PathTexGenNV: PFNGLPATHTEXGENNVPROC;
   PathFogGenNV: PFNGLPATHFOGGENNVPROC;
   CoverFillPathNV: PFNGLCOVERFILLPATHNVPROC;
   CoverStrokePathNV: PFNGLCOVERSTROKEPATHNVPROC;
   CoverFillPathInstancedNV: PFNGLCOVERFILLPATHINSTANCEDNVPROC;
   CoverStrokePathInstancedNV: PFNGLCOVERSTROKEPATHINSTANCEDNVPROC;
   GetPathParameterivNV: PFNGLGETPATHPARAMETERIVNVPROC;
   GetPathParameterfvNV: PFNGLGETPATHPARAMETERFVNVPROC;
   GetPathCommandsNV: PFNGLGETPATHCOMMANDSNVPROC;
   GetPathCoordsNV: PFNGLGETPATHCOORDSNVPROC;
   GetPathDashArrayNV: PFNGLGETPATHDASHARRAYNVPROC;
   GetPathMetricsNV: PFNGLGETPATHMETRICSNVPROC;
   GetPathMetricRangeNV: PFNGLGETPATHMETRICRANGENVPROC;
   GetPathSpacingNV: PFNGLGETPATHSPACINGNVPROC;
   GetPathColorGenivNV: PFNGLGETPATHCOLORGENIVNVPROC;
   GetPathColorGenfvNV: PFNGLGETPATHCOLORGENFVNVPROC;
   GetPathTexGenivNV: PFNGLGETPATHTEXGENIVNVPROC;
   GetPathTexGenfvNV: PFNGLGETPATHTEXGENFVNVPROC;
   IsPointInFillPathNV: PFNGLISPOINTINFILLPATHNVPROC;
   IsPointInStrokePathNV: PFNGLISPOINTINSTROKEPATHNVPROC;
   GetPathLengthNV: PFNGLGETPATHLENGTHNVPROC;
   PointAlongPathNV: PFNGLPOINTALONGPATHNVPROC;
   PathStencilDepthOffsetNV: PFNGLPATHSTENCILDEPTHOFFSETNVPROC;
   PathCoverDepthFuncNV: PFNGLPATHCOVERDEPTHFUNCNVPROC;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}
{$REGION 'Windows OpenGL (WGL) function/procedure definitions for ARB approved extensions'}
{$ENDIF}
{$IFDEF SUPPORT_WGL}
    // ###########################################################
    // function and procedure definitions for
    // ARB approved WGL extensions
    // ###########################################################

    // ARB approved WGL extension checks
    W_ARB_buffer_region, W_ARB_create_context, W_ARB_create_context_profile,
    W_ARB_extensions_string, W_ARB_framebuffer_sRGB, W_ARB_make_current_read,
    W_ARB_multisample, W_ARB_pbuffer, W_ARB_pixel_format, W_ARB_pixel_format_float,
    W_ARB_render_texture,

    // Vendor/EXT WGL extension checks
    W_ATI_pixel_format_float, W_EXT_framebuffer_sRGB,
    W_EXT_pixel_format_packed_float, W_EXT_swap_control, W_NV_gpu_affinity,
    W_EXT_create_context_es2_profile, W_NV_DX_interop: boolean;

    // WGL_buffer_region (ARB #4)
    WCreateBufferRegionARB: PFNWGLCREATEBUFFERREGIONARBPROC;
    WDeleteBufferRegionARB: PFNWGLDELETEBUFFERREGIONARBPROC;
    WSaveBufferRegionARB: PFNWGLSAVEBUFFERREGIONARBPROC;
    WRestoreBufferRegionARB: PFNWGLRESTOREBUFFERREGIONARBPROC;

    // WGL_ARB_extensions_string (ARB #8)
    WGetExtensionsStringARB: PFNWGLGETEXTENSIONSSTRINGARBPROC;

    // WGL_ARB_pixel_format (ARB #9)
    WGetPixelFormatAttribivARB: PFNWGLGETPIXELFORMATATTRIBIVARBPROC;
    WGetPixelFormatAttribfvARB: PFNWGLGETPIXELFORMATATTRIBFVARBPROC;
    WChoosePixelFormatARB: PFNWGLCHOOSEPIXELFORMATARBPROC;

    // WGL_make_current_read (ARB #10)
    WMakeContextCurrentARB: PFNWGLMAKECONTEXTCURRENTARBPROC;
    WGetCurrentReadDCARB: PFNWGLGETCURRENTREADDCARBPROC;

    // WGL_ARB_pbuffer (ARB #11)
    WCreatePbufferARB: PFNWGLCREATEPBUFFERARBPROC;
    WGetPbufferDCARB: PFNWGLGETPBUFFERDCARBPROC;
    WReleasePbufferDCARB: PFNWGLRELEASEPBUFFERDCARBPROC;
    WDestroyPbufferARB: PFNWGLDESTROYPBUFFERARBPROC;
    WQueryPbufferARB: PFNWGLQUERYPBUFFERARBPROC;

    // WGL_ARB_render_texture (ARB #20)
    WBindTexImageARB: PFNWGLBINDTEXIMAGEARBPROC;
    WReleaseTexImageARB: PFNWGLRELEASETEXIMAGEARBPROC;
    WSetPbufferAttribARB: PFNWGLSETPBUFFERATTRIBARBPROC;

    // WGL_ARB_create_context (ARB #55)
    WCreateContextAttribsARB: PFNWGLCREATECONTEXTATTRIBSARBPROC;
    // WGL_NV_gpu_affinity
    WEnumGpusNV: PFNWGLENUMGPUSNVPROC;
    WEnumGpuDevicesNV: PFNWGLENUMGPUDEVICESNVPROC;
    WCreateAffinityDCNV: PFNWGLCREATEAFFINITYDCNVPROC;
    WEnumGpusFromAffinityDCNV: PFNWGLENUMGPUSFROMAFFINITYDCNVPROC;
    WDeleteDCNV: PFNWGLDELETEDCNVPROC;

    // WGL_NV_DX_interop (EXT #407)
    WDXSetResourceShareHandleNV: PFNWGLDXSETRESOURCESHAREHANDLEPROC;
    WDXOpenDeviceNV: PFNWGLDXOPENDEVICEPROC;
    WDXCloseDeviceNV: PFNWGLDXCLOSEDEVICEPROC;
    WDXRegisterObjectNV: PFNWGLDXREGISTEROBJECTPROC;
    WDXUnregisterObjectNV: PFNWGLDXUNREGISTEROBJECTPROC;
    WDXObjectAccessNV: PFNWGLDXOBJECTACCESSPROC;
    WDXLockObjectsNV: PFNWGLDXLOCKOBJECTSPROC;
    WDXUnlockObjectsNV: PFNWGLDXUNLOCKOBJECTSNVPROC;

{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}
{$REGION 'Windows OpenGL (WGL) function/procedure definitions for Vendor/EXT extensions'}
{$ENDIF}
{$IFDEF SUPPORT_WGL}
    // ###########################################################
    // function and procedure definitions for
    // Vendor/EXT WGL extensions
    // ###########################################################

    // WGL_EXT_swap_control (EXT #172)
    WSwapIntervalEXT: PFNWGLSWAPINTERVALEXTPROC;
    WGetSwapIntervalEXT: PFNWGLGETSWAPINTERVALEXTPROC;

    // GL_NV_vertex_array_range (EXT #190)
    WAllocateMemoryNV: PFNWGLALLOCATEMEMORYNVPROC;
    WFreeMemoryNV: PFNWGLFREEMEMORYNVPROC;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'GLX function/procedure definitions for ARB approved extensions'}
{$ENDIF}
{$IFDEF SUPPORT_GLX}
    // ###########################################################
    // function and procedure definitions for
    // ARB approved GLX extensions
    // ###########################################################

    // GLX extension checks
    X_VERSION_1_1, X_VERSION_1_2, X_VERSION_1_3, X_VERSION_1_4,
    X_ARB_create_context, X_ARB_create_context_profile, X_ARB_framebuffer_sRGB,
    X_ARB_multisample, X_EXT_framebuffer_sRGB, X_EXT_fbconfig_packed_float,
    X_SGIS_multisample, X_EXT_visual_info, X_SGI_swap_control,
    X_SGI_video_sync, X_SGI_make_current_read, X_SGIX_video_source,
    X_EXT_visual_rating, X_EXT_import_context, X_SGIX_fbconfig, X_SGIX_pbuffer,
    X_SGI_cushion, X_SGIX_video_resize, X_SGIX_dmbuffer, X_SGIX_swap_group,
    X_SGIX_swap_barrier, X_SGIS_blended_overlay, X_SGIS_shared_multisample,
    X_SUN_get_transparent_index, X_3DFX_multisample, X_MESA_copy_sub_buffer,
    X_MESA_pixmap_colormap, X_MESA_release_buffers, X_MESA_set_3dfx_mode,
    X_SGIX_visual_select_group, X_SGIX_hyperpipe, X_NV_multisample_coverage: boolean;

    // GLX 1.3 and later
    XChooseFBConfig: PFNGLXCHOOSEFBCONFIGPROC;
    XGetFBConfigAttrib: PFNGLXGETFBCONFIGATTRIBPROC;
    XGetFBConfigs: PFNGLXGETFBCONFIGSPROC;
    XGetVisualFromFBConfig: PFNGLXGETVISUALFROMFBCONFIGPROC;
    XCreateWindow: PFNGLXCREATEWINDOWPROC;
    XDestroyWindow: PFNGLXDESTROYWINDOWPROC;
    XCreatePixmap: PFNGLXCREATEPIXMAPPROC;
    XDestroyPixmap: PFNGLXDESTROYPIXMAPPROC;
    XCreatePbuffer: PFNGLXCREATEPBUFFERPROC;
    XDestroyPbuffer: PFNGLXDESTROYPBUFFERPROC;
    XQueryDrawable: PFNGLXQUERYDRAWABLEPROC;
    XCreateNewContext: PFNGLXCREATENEWCONTEXTPROC;
    XMakeContextCurrent: PFNGLXMAKECONTEXTCURRENTPROC;
    XGetCurrentReadDrawable: PFNGLXGETCURRENTREADDRAWABLEPROC;
    XQueryContext: PFNGLXQUERYCONTEXTPROC;
    XSelectEvent: PFNGLXSELECTEVENTPROC;
    XGetSelectedEvent: PFNGLXGETSELECTEDEVENTPROC;
    XBindTexImageARB: PFNGLXBINDTEXIMAGEARBPROC;
    XReleaseTexImageARB: PFNGLXRELEASETEXIMAGEARBPROC;
    XDrawableAttribARB: PFNGLXDRAWABLEATTRIBARBPROC;

    // GLX 1.4
    // X_ARB_create_context (EXT #56)
    XCreateContextAttribsARB: PFNGLXCREATECONTEXTATTRIBSARBPROC;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'GLX function/procedure definitions for Vendor/EXT extensions'}
{$ENDIF}
{$IFDEF SUPPORT_GLX}
    // ###########################################################
    // function and procedure definitions for
    // Vendor/EXT GLX extensions
    // ###########################################################

    // X_SGI_swap_control (EXT #40)
    XSwapIntervalSGI: PFNGLXSWAPINTERVALSGIPROC;
    XGetVideoSyncSGI: PFNGLXGETVIDEOSYNCSGIPROC;
    XWaitVideoSyncSGI: PFNGLXWAITVIDEOSYNCSGIPROC;
    XFreeContextEXT: PFNGLXFREECONTEXTEXTPROC;
    XGetContextIDEXT: PFNGLXGETCONTEXTIDEXTPROC;
    XGetCurrentDisplayEXT: PFNGLXGETCURRENTDISPLAYEXTPROC;
    XImportContextEXT: PFNGLXIMPORTCONTEXTEXTPROC;
    XQueryContextInfoEXT: PFNGLXQUERYCONTEXTINFOEXTPROC;
    XCopySubBufferMESA: PFNGLXCOPYSUBBUFFERMESAPROC;
    XCreateGLXPixmapMESA: PFNGLXCREATEGLXPIXMAPMESAPROC;
    XReleaseBuffersMESA: PFNGLXRELEASEBUFFERSMESAPROC;
    XSet3DfxModeMESA: PFNGLXSET3DFXMODEMESAPROC;

    XBindTexImageEXT: PFNGLXBINDTEXIMAGEEXTPROC;
    XReleaseTexImageEXT: PFNGLXRELEASETEXIMAGEEXTPROC;

    // GLX 1.4
    XMakeCurrentReadSGI: PFNGLXMAKECURRENTREADSGIPROC;
    XGetCurrentReadDrawableSGI: PFNGLXGETCURRENTREADDRAWABLESGIPROC;
    XGetFBConfigAttribSGIX: PFNGLXGETFBCONFIGATTRIBSGIXPROC;
    XChooseFBConfigSGIX: PFNGLXCHOOSEFBCONFIGSGIXPROC;
    XCreateGLXPixmapWithConfigSGIX: PFNGLXCREATEGLXPIXMAPWITHCONFIGSGIXPROC;
    XCreateContextWithConfigSGIX: PFNGLXCREATECONTEXTWITHCONFIGSGIXPROC;
    XGetVisualFromFBConfigSGIX: PFNGLXGETVISUALFROMFBCONFIGSGIXPROC;
    XGetFBConfigFromVisualSGIX: PFNGLXGETFBCONFIGFROMVISUALSGIXPROC;
    XCreateGLXPbufferSGIX: PFNGLXCREATEGLXPBUFFERSGIXPROC;
    XDestroyGLXPbufferSGIX: PFNGLXDESTROYGLXPBUFFERSGIXPROC;
    XQueryGLXPbufferSGIX: PFNGLXQUERYGLXPBUFFERSGIXPROC;
    XSelectEventSGIX: PFNGLXSELECTEVENTSGIXPROC;
    XGetSelectedEventSGIX: PFNGLXGETSELECTEDEVENTSGIXPROC;
    XCushionSGI: PFNGLXCUSHIONSGIPROC;
    XBindChannelToWindowSGIX: PFNGLXBINDCHANNELTOWINDOWSGIXPROC;
    XChannelRectSGIX: PFNGLXCHANNELRECTSGIXPROC;
    XQueryChannelRectSGIX: PFNGLXQUERYCHANNELRECTSGIXPROC;
    XQueryChannelDeltasSGIX: PFNGLXQUERYCHANNELDELTASSGIXPROC;
    XChannelRectSyncSGIX: PFNGLXCHANNELRECTSYNCSGIXPROC;
    XJoinSwapGroupSGIX: PFNGLXJOINSWAPGROUPSGIXPROC;
    XBindSwapBarrierSGIX: PFNGLXBINDSWAPBARRIERSGIXPROC;
    XQueryMaxSwapBarriersSGIX: PFNGLXQUERYMAXSWAPBARRIERSSGIXPROC;
    XQueryHyperpipeNetworkSGIX: PFNGLXQUERYHYPERPIPENETWORKSGIXPROC;
    XHyperpipeConfigSGIX: PFNGLXHYPERPIPECONFIGSGIXPROC;
    XQueryHyperpipeConfigSGIX: PFNGLXQUERYHYPERPIPECONFIGSGIXPROC;
    XDestroyHyperpipeConfigSGIX: PFNGLXDESTROYHYPERPIPECONFIGSGIXPROC;
    XBindHyperpipeSGIX: PFNGLXBINDHYPERPIPESGIXPROC;
    XQueryHyperpipeBestAttribSGIX: PFNGLXQUERYHYPERPIPEBESTATTRIBSGIXPROC;
    XHyperpipeAttribSGIX: PFNGLXHYPERPIPEATTRIBSGIXPROC;
    XQueryHyperpipeAttribSGIX: PFNGLXQUERYHYPERPIPEATTRIBSGIXPROC;
    XGetAGPOffsetMESA: PFNGLXGETAGPOFFSETMESAPROC;
    XEnumerateVideoDevicesNV: PFNGLXENUMERATEVIDEODEVICESNVPROC;
    XBindVideoDeviceNV: PFNGLXBINDVIDEODEVICENVPROC;
    XGetVideoDeviceNV: PFNGLXGETVIDEODEVICENVPROC;

    XAllocateMemoryNV: PFNGLXALLOCATEMEMORYNVPROC;
    XFreeMemoryNV: PFNGLXFREEMEMORYNVPROC;

    XReleaseVideoDeviceNV: PFNGLXRELEASEVIDEODEVICENVPROC;
    XBindVideoImageNV: PFNGLXBINDVIDEOIMAGENVPROC;
    XReleaseVideoImageNV: PFNGLXRELEASEVIDEOIMAGENVPROC;
    XSendPbufferToVideoNV: PFNGLXSENDPBUFFERTOVIDEONVPROC;
    XGetVideoInfoNV: PFNGLXGETVIDEOINFONVPROC;
    XJoinSwapGroupNV: PFNGLXJOINSWAPGROUPNVPROC;
    XBindSwapBarrierNV: PFNGLXBINDSWAPBARRIERNVPROC;
    XQuerySwapGroupNV: PFNGLXQUERYSWAPGROUPNVPROC;
    XQueryMaxSwapGroupsNV: PFNGLXQUERYMAXSWAPGROUPSNVPROC;
    XQueryFrameCountNV: PFNGLXQUERYFRAMECOUNTNVPROC;
    XResetFrameCountNV: PFNGLXRESETFRAMECOUNTNVPROC;
    XBindVideoCaptureDeviceNV: PFNGLXBINDVIDEOCAPTUREDEVICENVPROC;
    XEnumerateVideoCaptureDevicesNV: PFNGLXENUMERATEVIDEOCAPTUREDEVICESNVPROC;
    XLockVideoCaptureDeviceNV: PFNGLXLOCKVIDEOCAPTUREDEVICENVPROC;
    XQueryVideoCaptureDeviceNV: PFNGLXQUERYVIDEOCAPTUREDEVICENVPROC;
    XReleaseVideoCaptureDeviceNV: PFNGLXRELEASEVIDEOCAPTUREDEVICENVPROC;
    XSwapIntervalEXT: PFNGLXSWAPINTERVALEXTPROC;
    XCopyImageSubDataNV: PFNGLXCOPYIMAGESUBDATANVPROC;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'AGL function/procedure'}{$ENDIF}
{$IFDEF DARWIN}
    // AGL extension checks
    A_aux_depth_stencil,A_client_storage,A_element_array,A_fence,A_float_pixels,
    A_flush_buffer_range,A_flush_render,A_object_purgeable,A_packed_pixels,
    A_pixel_buffer,A_rgb_422,A_specular_vector,A_texture_range,A_transform_hint,
    A_vertex_array_object,A_vertex_array_range,A_vertex_program_evaluators,
    A_ycbcr_422: boolean;

    ACreatePixelFormat: function(gdevs: PAGLDevice; ndev: GLint; attribs: PGLint): TAGLPixelFormat; cdecl;
    AChoosePixelFormat : function(gdevs: PAGLDevice; ndev: GLint; attribs: PGLint): TAGLPixelFormat; cdecl;
    ADestroyPixelFormat: procedure(pix: TAGLPixelFormat); cdecl;
    ADescribePixelFormat : function(pix: TAGLPixelFormat; attrib: TGLint; value: PGLint): TGLBoolean; cdecl;
    AGetCGLPixelFormat : function(pix: TAGLPixelFormat; cgl_pix: Pointer): TGLboolean; cdecl;
    ADisplaysOfPixelFormat : function(pix: TAGLPixelFormat; ndevs: PGLint): CGDirectDisplayID; cdecl;
    ANextPixelFormat : function(pix: TAGLPixelFormat): TAGLPixelFormat; cdecl;
    // Managing context
    ACreateContext: function(pix: TAGLPixelFormat; share: TAGLContext): TAGLContext; cdecl;
    ACopyContext : function(src: TAGLContext; dst: TAGLContext; mask: TGLuint): TGLBoolean; cdecl;
    ADestroyContext: function(ctx: TAGLContext): GLboolean; cdecl;
    AUpdateContext: function(ctx: TAGLContext): GLboolean; cdecl;
    ASetCurrentContext : function(ctx: TAGLContext): GLboolean; cdecl;
    AGetCGLContext : function(ctx: TAGLContext; cgl_ctx: Pointer): GLboolean; cdecl;
    AGetCurrentContext : function(): TAGLContext; cdecl;
    ASwapBuffers : procedure(ctx: TAGLContext); cdecl;
    // Managing Pixel Buffers
    ACreatePBuffer : function(Width: GLint; Height: GLint; target: GLenum; internalFormat: GLenum;
      max_level: longint; pbuffer: PAGLPbuffer): GLboolean; cdecl;
    ADestroyPBuffer : function(pbuffer: TAGLPbuffer): GLboolean; cdecl;
    ADescribePBuffer : function(pbuffer: TAGLPbuffer; width, height: PGLint; target: PGLenum; internalFormat: PGLenum; max_level: PGLint): TGLBoolean; cdecl;
    AGetPBuffer : function(ctx: TAGLContext; out pbuffer: TAGLPbuffer; face, level, screen: PGLint): GLboolean; cdecl;
    ASetPBuffer : function(ctx: TAGLContext; pbuffer: TAGLPbuffer; face: GLint; level: GLint; screen: GLint): GLboolean; cdecl;
    ATexImagePBuffer : function(ctx: TAGLContext; pbuffer: TAGLPbuffer; source: GLint): TGLBoolean; cdecl;
    // Managing Drawable Objects
    ASetDrawable: function(ctx: TAGLContext; draw: TAGLDrawable): GLboolean; cdecl; // deprecated
    AGetDrawable: function(ctx: TAGLContext): TAGLDrawable; cdecl; // deprecated
    ASetFullScreen: function(ctx: TAGLContext; Width: GLsizei; Height: GLsizei; freq: GLsizei;
      device: GLint): GLboolean; cdecl;
    ASetOffScreen : function(ctx: TAGLContext; width, height, rowbytes: TGLsizei; out baseaddr: Pointer): GLboolean; cdecl;
    // Getting and Setting Context Options
    AEnable : function(ctx: TAGLContext; pname: TGLenum): GLboolean; cdecl;
    ADisable : function(ctx: TAGLContext; pname: TGLenum): GLboolean; cdecl;
    AIsEnabled : function(ctx: TAGLContext; pname: TGLenum): GLboolean; cdecl;
    ASetInteger : function(ctx: TAGLContext; pname: GLenum; params: PGLint): GLboolean; cdecl;
    AGetInteger : function(ctx: TAGLContext; pname: GLenum; params: PGLint): GLboolean; cdecl;
    // Getting and Setting Global Information
    AConfigure : function(pname: TGLenum; param: TGLuint): TGLboolean; cdecl;
    AGetVersion : procedure(major: PGLint; minor: PGLint); cdecl;
    AResetLibrary : procedure(); cdecl;
    // Getting Renderer Information
    ADescribeRenderer : function(rend: TAGLRendererInfo; prop: GLint; value: PGLint): GLboolean; cdecl;
    ADestroyRendererInfo : procedure(rend: TAGLRendererInfo); cdecl;
    ANextRendererInfo : function(rend: TAGLRendererInfo): TAGLRendererInfo; cdecl;
    AQueryRendererInfoForCGDirectDisplayIDs : function(dspIDs: CGDirectDisplayID; ndev: TGLint): TAGLRendererInfo; cdecl;
    // Managing Virtual Screens
    AGetVirtualScreen : function(ctx: TAGLContext): GLint; cdecl;
    ASetVirtualScreen : function(ctx: TAGLContext; screen: TGLint): TGLboolean; cdecl;
    // Getting and Setting Windows
    ASetWindowRef : function(ctx: TAGLContext; window: WindowRef): TGLBoolean; cdecl;
    AGetWindowRef : function(ctx: TAGLContext): TGLint; cdecl;
    // Getting and Setting HIView Objects
    ASetHIViewRef : function(ctx: TAGLContext; hiview: HIViewRef): TGLboolean; cdecl;
    AGetHIViewRef : function(ctx: TAGLContext): HIViewRef; cdecl;
    // Getting Error Information
    AGetError : function(): TGLenum; cdecl;
    AErrorString : function(code: TGLenum): PGLChar; cdecl;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'locate functions/procedures for OpenGL Utility (GLU) extensions'} {$ENDIF}

    // ###########################################################
    // locate functions and procedures for
    // GLU extensions
    // ###########################################################

    UNurbsCallbackDataEXT:
    procedure(nurb: PGLUnurbs; userData: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    UNewNurbsTessellatorEXT:
    function: PGLUnurbs;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    UDeleteNurbsTessellatorEXT:
    procedure(nurb: PGLUnurbs);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
{$IFDEF GLS_REGIONS} {$ENDREGION} {$ENDIF}
    constructor Create;
    procedure Initialize(ATemporary: boolean = False);
    procedure Close;
    procedure CheckError;
    procedure ClearError;
    property IsInitialized: boolean read FInitialized;
    property DebugMode: boolean read FDebug write FDebug;
  end;

{$IFDEF GLS_REGIONS}{$REGION 'Windows OpenGL (WGL) support functions'}{$ENDIF}
{$IFDEF SUPPORT_WGL}

function wglGetProcAddress(ProcName: PGLChar): Pointer; stdcall; external opengl32;
function wglCopyContext(p1: HGLRC; p2: HGLRC; p3: cardinal): BOOL;
  stdcall; external opengl32;
function wglCreateContext(DC: HDC): HGLRC; stdcall; external opengl32;
function wglCreateLayerContext(p1: HDC; p2: integer): HGLRC; stdcall; external opengl32;
function wglDeleteContext(p1: HGLRC): BOOL; stdcall; external opengl32;
function wglDescribeLayerPlane(p1: HDC; p2, p3: integer; p4: cardinal;
  var p5: TLayerPlaneDescriptor): BOOL; stdcall; external opengl32;
function wglGetCurrentContext: HGLRC; stdcall; external opengl32;
function wglGetCurrentDC: HDC; stdcall; external opengl32;
function wglGetLayerPaletteEntries(p1: HDC; p2, p3, p4: integer; var pcr): integer;
  stdcall; external opengl32;
function wglMakeCurrent(DC: HDC; p2: HGLRC): BOOL; stdcall; external opengl32;
function wglRealizeLayerPalette(p1: HDC; p2: integer; p3: BOOL): BOOL;
  stdcall; external opengl32;
function wglSetLayerPaletteEntries(p1: HDC; p2, p3, p4: integer; var pcr): integer;
  stdcall; external opengl32;
function wglShareLists(p1, p2: HGLRC): BOOL; stdcall; external opengl32;
function wglSwapLayerBuffers(p1: HDC; p2: cardinal): BOOL; stdcall; external opengl32;
function wglSwapMultipleBuffers(p1: UINT; const p2: PWGLSwap): DWORD;
  stdcall; external opengl32;
function wglUseFontBitmapsA(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  external opengl32;
function wglUseFontOutlinesA(p1: HDC; p2, p3, p4: DWORD; p5, p6: single;
  p7: integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
function wglUseFontBitmapsW(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  external opengl32;
function wglUseFontOutlinesW(p1: HDC; p2, p3, p4: DWORD; p5, p6: single;
  p7: integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
function wglUseFontBitmaps(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  external opengl32 Name 'wglUseFontBitmapsA';
function wglUseFontOutlines(p1: HDC; p2, p3, p4: DWORD; p5, p6: single;
  p7: integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  external opengl32 Name 'wglUseFontOutlinesA';
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'OpenGL Extension to the X Window System (GLX) support functions'}
{$ENDIF}
{$IFDEF SUPPORT_GLX}
// GLX 1.0
function glXGetProcAddress(const Name: PAnsiChar): Pointer; cdecl; external opengl32;
function glXGetProcAddressARB(const Name: PAnsiChar): Pointer; cdecl; external opengl32;
function glXChooseVisual(dpy: PDisplay; screen: TGLint;
  attribList: PGLint): PXVisualInfo; cdecl; external opengl32;
function glXCreateContext(dpy: PDisplay; vis: PXVisualInfo;
  shareList: GLXContext; direct: TGLboolean): GLXContext; cdecl; external opengl32;
procedure glXDestroyContext(dpy: PDisplay; ctx: GLXContext); cdecl; external opengl32;
function glXMakeCurrent(dpy: PDisplay; drawable: GLXDrawable;
  ctx: GLXContext): TGLboolean; cdecl; external opengl32;
procedure glXCopyContext(dpy: PDisplay; src: GLXContext; dst: GLXContext; mask: TGLuint);
  cdecl; external opengl32;
procedure glXSwapBuffers(dpy: PDisplay; drawable: GLXDrawable); cdecl; external opengl32;
function glXCreateGLXPixmap(dpy: PDisplay; visual: PXVisualInfo;
  pixmap: GLXPixmap): GLXPixmap; cdecl; external opengl32;
procedure glXDestroyGLXPixmap(dpy: PDisplay; pixmap: GLXPixmap);
  cdecl; external opengl32;
function glXQueryExtension(dpy: PDisplay; errorb: PGLint; event: PGLint): TGLboolean;
  cdecl; external opengl32;
function glXQueryVersion(dpy: PDisplay; maj: PGLint; min: PGLint): TGLboolean;
  cdecl; external opengl32;
function glXIsDirect(dpy: PDisplay; ctx: GLXContext): TGLboolean;
  cdecl; external opengl32;
function glXGetConfig(dpy: PDisplay; visual: PXVisualInfo; attrib: TGLint;
  Value: PGLint): TGLint; cdecl; external opengl32;
function glXGetCurrentContext: GLXContext; cdecl; external opengl32;
function glXGetCurrentDrawable: GLXDrawable; cdecl; external opengl32;
procedure glXWaitGL; cdecl; external opengl32;
procedure glXWaitX; cdecl; external opengl32;
procedure glXUseXFont(font: XFont; First: TGLint; Count: TGLint; list: TGLint);
  cdecl; external opengl32;

// GLX 1.1 and later
function glXQueryExtensionsString(dpy: PDisplay; screen: TGLint): PGLChar;
  cdecl; external opengl32;
function glXQueryServerString(dpy: PDisplay; screen: TGLint; Name: TGLint): PGLChar;
  cdecl; external opengl32;
function glXGetClientString(dpy: PDisplay; Name: TGLint): PGLChar;
  cdecl; external opengl32;

// GLX 1.2 and later
function glXGetCurrentDisplay: PDisplay; cdecl; external opengl32;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'OpenGL Extension to the Apple System (AGL) support functions'}
{$ENDIF}
{$IFDEF DARWIN}
function aglChoosePixelFormat(gdevs: PAGLDevice; ndev: GLint; attribs: PGLint): TAGLPixelFormat; cdecl; external libAGL;
procedure aglDestroyPixelFormat(pix: TAGLPixelFormat); cdecl; external libAGL;

function aglCreateContext(pix: TAGLPixelFormat; share: TAGLContext): TAGLContext; cdecl; external libAGL;
function aglDestroyContext(ctx: TAGLContext): GLboolean; cdecl; external libAGL;
function aglSetCurrentContext (ctx: TAGLContext): GLboolean; cdecl; external libAGL;
function aglSetDrawable (ctx: TAGLContext; draw: TAGLDrawable): GLboolean; cdecl; external libAGL;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS}{$REGION 'EGL function/procedure'}{$ENDIF}
{$IFDEF GLS_OPENGL_ES}
var
    eglGetError : function:EGLint;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglGetDisplay : function(display_id:EGLNativeDisplayType):EGLDisplay;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglInitialize : function(dpy:EGLDisplay; major:pEGLint; minor:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglTerminate : function(dpy:EGLDisplay):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglQueryString : function(dpy:EGLDisplay; name:EGLint):pchar;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglGetConfigs : function(dpy:EGLDisplay; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglChooseConfig : function(dpy:EGLDisplay; attrib_list:pEGLint; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglGetConfigAttrib : function(dpy:EGLDisplay; config:EGLConfig; attribute:EGLint; value:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglCreateWindowSurface : function(dpy:EGLDisplay; config:EGLConfig; win:EGLNativeWindowType; attrib_list:pEGLint):EGLSurface;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglCreatePbufferSurface : function(dpy:EGLDisplay; config:EGLConfig; attrib_list:pEGLint):EGLSurface;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglCreatePixmapSurface : function(dpy:EGLDisplay; config:EGLConfig; pixmap:EGLNativePixmapType; attrib_list:pEGLint):EGLSurface;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglDestroySurface : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglQuerySurface : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglBindAPI : function(api:EGLenum):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglQueryAPI : function:EGLenum;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglWaitClient : function:EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglReleaseThread : function:EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglCreatePbufferFromClientBuffer : function(dpy:EGLDisplay; buftype:EGLenum; buffer:EGLClientBuffer; config:EGLConfig; attrib_list:pEGLint):EGLSurface;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglSurfaceAttrib : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:EGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglBindTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglReleaseTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglSwapInterval : function(dpy:EGLDisplay; interval:EGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglCreateContext : function(dpy:EGLDisplay; config:EGLConfig; share_context:EGLContext; attrib_list:pEGLint):EGLContext;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglDestroyContext : function(dpy:EGLDisplay; ctx:EGLContext):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglMakeCurrent : function(dpy:EGLDisplay; draw:EGLSurface; read:EGLSurface; ctx:EGLContext):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglGetCurrentContext : function:EGLContext;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglGetCurrentSurface : function(readdraw:EGLint):EGLSurface;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglGetCurrentDisplay : function:EGLDisplay;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglQueryContext : function(dpy:EGLDisplay; ctx:EGLContext; attribute:EGLint; value:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglWaitGL : function:EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglWaitNative : function(engine:EGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglSwapBuffers : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    eglCopyBuffers : function(dpy:EGLDisplay; surface:EGLSurface; target:EGLNativePixmapType):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}

{$ENDIF GLS_OPENGL_ES}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}

{$IFDEF GLS_REGIONS} {$REGION 'OpenGL utility (GLU) functions and procedures'}
 {$ENDIF}
function gluErrorString(errCode: TGLEnum): PGLChar; {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
function gluGetString(Name: TGLEnum): PGLChar; {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluOrtho2D(left, right, bottom, top: TGLdouble);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluPerspective(fovy, aspect, zNear, zFar: TGLdouble);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluPickMatrix(X, y, Width, Height: TGLdouble; const Viewport: TVector4i);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluLookAt(eyex, eyey, eyez, centerx, centery, centerz, upx,
  upy, upz: TGLdouble);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
function gluProject(objx, objy, objz: TGLdouble; const modelMatrix: TMatrix4d;
  const projMatrix: TMatrix4d; const Viewport: TVector4i;
  winx, winy, winz: PGLdouble): TGLint; {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
function gluUnProject(winx, winy, winz: TGLdouble; const modelMatrix: TMatrix4d;
  const projMatrix: TMatrix4d; const Viewport: TVector4i;
  objx, objy, objz: PGLdouble): TGLint; {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
function gluScaleImage(format: TGLEnum; widthin, heightin: TGLint;
  typein: TGLEnum; datain: Pointer; widthout, heightout: TGLint;
  typeout: TGLEnum; dataout: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
function gluBuild1DMipmaps(target: TGLEnum; Components, Width: TGLint;
  format, atype: TGLEnum; Data: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
function gluBuild2DMipmaps(target: TGLEnum; Components, Width, Height: TGLint;
  format, atype: TGLEnum; Data: Pointer): TGLint; {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
function gluNewQuadric: PGLUquadric; {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluDeleteQuadric(state: PGLUquadric); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluQuadricNormals(quadObject: PGLUquadric; normals: TGLEnum);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluQuadricTexture(quadObject: PGLUquadric; textureCoords: TGLboolean);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluQuadricOrientation(quadObject: PGLUquadric; orientation: TGLEnum);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluQuadricDrawStyle(quadObject: PGLUquadric; drawStyle: TGLEnum);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluCylinder(quadObject: PGLUquadric; baseRadius, topRadius, Height: TGLdouble;
  slices, stacks: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl;
 {$ENDIF} external glu32;
procedure gluDisk(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble;
  slices, loops: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl;
 {$ENDIF} external glu32;
procedure gluPartialDisk(quadObject: PGLUquadric; innerRadius, outerRadius: TGLdouble;
  slices, loops: TGLint; startAngle, sweepAngle: TGLdouble);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluSphere(quadObject: PGLUquadric; radius: TGLdouble; slices, stacks: TGLint);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluQuadricCallback(quadObject: PGLUquadric; which: TGLEnum;
  fn: TGLUQuadricErrorProc); {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl;
 {$ENDIF} external glu32;

{$IFNDEF ANDROID}

function gluNewTess: PGLUtesselator; {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluDeleteTess(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluTessBeginPolygon(tess: PGLUtesselator; polygon_data: Pointer);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluTessBeginContour(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluTessVertex(tess: PGLUtesselator; const coords: TVector3d; Data: Pointer);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluTessEndContour(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluTessEndPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluTessProperty(tess: PGLUtesselator; which: TGLEnum; Value: TGLdouble);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluTessNormal(tess: PGLUtesselator; X, y, z: TGLdouble);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluTessCallback(tess: PGLUtesselator; which: TGLEnum; fn: Pointer);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluGetTessProperty(tess: PGLUtesselator; which: TGLEnum; Value: PGLdouble);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
function gluNewNurbsRenderer: PGLUnurbs; {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluDeleteNurbsRenderer(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;

{$ENDIF}

procedure gluBeginSurface(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluBeginCurve(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluEndCurve(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluEndSurface(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluBeginTrim(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluEndTrim(nobj: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluPwlCurve(nobj: PGLUnurbs; Count: TGLint; points: PGLfloat;
  stride: TGLint; atype: TGLEnum); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluNurbsCurve(nobj: PGLUnurbs; nknots: TGLint; knot: PGLfloat;
  stride: TGLint; ctlarray: PGLfloat; order: TGLint; atype: TGLEnum);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluNurbsSurface(nobj: PGLUnurbs; sknot_count: TGLint;
  sknot: PGLfloat; tknot_count: TGLint; tknot: PGLfloat; s_stride, t_stride: TGLint;
  ctlarray: PGLfloat; sorder, torder: TGLint; atype: TGLEnum);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluLoadSamplingMatrices(nobj: PGLUnurbs; const modelMatrix: TMatrix4f;
  const projMatrix: TMatrix4f; const Viewport: TVector4i);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluNurbsProperty(nobj: PGLUnurbs; aproperty: TGLEnum; Value: TGLfloat);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluGetNurbsProperty(nobj: PGLUnurbs; aproperty: TGLEnum; Value: PGLfloat);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluNurbsCallback(nobj: PGLUnurbs; which: TGLEnum; fn: TGLUNurbsErrorProc);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluBeginPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluNextContour(tess: PGLUtesselator; atype: TGLEnum);
 {$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;
procedure gluEndPolygon(tess: PGLUtesselator); {$IFDEF MSWINDOWS} stdcall;
 {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF} external glu32;

{$IFDEF GLS_REGIONS} {$ENDREGION} {$ENDIF}
function GLLibGetProcAddress(ProcName: PGLChar): Pointer;
function GLGetProcAddress(ProcName: PGLChar; glCap: Pointer): Pointer;

procedure CloseOpenGL;
function InitOpenGL: boolean;
function InitOpenGLFromLibrary(const GLName, GLUName: string): boolean;
function IsOpenGLInitialized: boolean;

// compatibility routines
procedure UnloadOpenGL;
function LoadOpenGL: boolean;
function LoadOpenGLFromLibrary(GLName, GLUName: string): boolean;
function IsOpenGLLoaded: boolean;

function IsMesaGL: boolean;
procedure TrimAndSplitVersionString(buffer: string; var max, min: integer);
function IsVersionMet(MajorVersion, MinorVersion, actualMajorVersion,
  actualMinorVersion: integer): boolean;

implementation

uses
{$IFDEF FPC}
  Math, LCLProc,
{$ENDIF}
  GLScene_Base_Log;

resourcestring
  rstrOpenGLError = 'OpenGL error - %s';

const
  glPrefix = 'gl';


procedure glCap(fn: string); {$IFDEF GLS_INLINE} inline; {$ENDIF}
begin
  GLSLogger.LogError('Call OpenGL function with undefined entry point:'+fn);
  Abort;
end;

{$I glсaps.inc}


// ************** Windows specific ********************
{$IFDEF MSWINDOWS}

const
  INVALID_MODULEHANDLE = 0;

var
  GLHandle: HINST;
  GLUHandle: HINST;
{$IFDEF GLS_OPENGL_ES}
  EGLHandle: HINST;
  EGL2Handle: HINST;
{$ENDIF}

function GLGetProcAddress(ProcName: PGLChar; glCap: Pointer): Pointer;
begin
{$IFNDEF GLS_OPENGL_ES}
  Result := wglGetProcAddress(ProcName);
{$ELSE}
  Result := GetProcAddress(EGL2Handle, ProcName);
{$ENDIF}
  if Result = nil then
    Result := glCap;
end;

{$IFDEF GLS_OPENGL_ES}
function EGLGetProcAddress(ProcName: PGLChar): Pointer;
begin
  Result := GetProcAddress(EGLHandle, ProcName);
end;
{$ENDIF}

{$ENDIF}
// ************** UNIX specific ********************
{$IFDEF UNIX}

const
  INVALID_MODULEHANDLE = 0; // nil;

var
  GLHandle: TLibHandle = 0; // Pointer;
  GLUHandle: TLibHandle = 0; // Pointer;
{$IFDEF GLS_OPENGL_ES}
  EGLHandle: TLibHandle = 0;
  GLES1Handle: TLibHandle = 0;    //libGLESv1_CM
  GLES2Handle: TLibHandle = 0;    //LibGLESv2
{$ENDIF}

{$IFDEF DARWIN}
  AGLHandle: TLibHandle = 0;
  dlHandle: TLibHandle = 0;
  {$IFDEF GLS_OPENGL_ES}
  EGL2Handle: TLibHandle = 0;
  {$ENDIF}
{$ENDIF}

{$IFDEF DARWIN}
function NSIsSymbolNameDefined(s: PChar): Bool; cdecl; external libdl;
function NSLookupAndBindSymbol(s: PChar): PtrInt; cdecl; external libdl;
function NSAddressOfSymbol(Lib: pointer): PtrInt; cdecl; external libdl;

function GetProcAddress(Lib : TlibHandle; ProcName : AnsiString) : Pointer;
var
  fname: PChar;
  symbol: PtrInt;
begin
  fname := PChar('_' + ProcName);
  if not NSIsSymbolNameDefined(fname) then
    exit(nil);

  symbol := NSLookupAndBindSymbol(fname);
  if symbol <> 0 then
    symbol := NSAddressOfSymbol(Pointer(symbol));

  Result := Pointer(symbol);
end;
{$ENDIF}

function GLGetProcAddress(ProcName: PGLChar; glCap: Pointer): Pointer;
begin
{$IFNDEF GLS_OPENGL_ES}
  {$IFDEF SUPPORT_GLX}
  if @glXGetProcAddress <> nil then
    Result := glXGetProcAddress(ProcName);

  if Result <> nil then
    exit;

  if @glXGetProcAddressARB <> nil then
    Result := glXGetProcAddressARB(ProcName);

  {$ENDIF}
  Result := GetProcAddress(GLHandle, ProcName);
{$ELSE}
  Result := GetProcAddress(GLES2Handle, ProcName);
  if Result <> nil then
    exit;
  Result := GetProcAddress(GLES1Handle, ProcName);
{$ENDIF}
  if Result = nil then
    Result := glCap;
end;

{$IFDEF DARWIN}
function AGLGetProcAddress(ProcName: PGLChar): Pointer;
begin
  Result := GetProcAddress(AGLHandle, ProcName);
end;
{$ELSE}

{$IFDEF GLS_OPENGL_ES}
function EGLGetProcAddress(ProcName: PGLChar): Pointer;
begin
  Result := GetProcAddress(EGLHandle, ProcName);
end;
{$ENDIF}

{$ENDIF DARWIN}

{$ENDIF UNIX}

function GLLibGetProcAddress(ProcName: PGLChar): Pointer;
begin
  Result := GetProcAddress(GLHandle, ProcName);
end;

var
  vNotInformed: boolean = True;

procedure DebugCallBack(Source: GLenum; type_: GLenum; id: GLuint;
  severity: GLenum; length: GLSizei; const message: PGLChar; userParam: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  if length > 0 then
    GLSLogger.LogDebug(string(message));
end;

procedure DebugCallBackAMD(id: GLuint; category: GLenum; severity: GLenum;
  length: GLSizei; message: PGLChar; userParam: Pointer);
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
  if length > 0 then
    GLSLogger.LogDebug(string(message));
end;

constructor TGLExtensionsAndEntryPoints.Create;
begin
  FInitialized := False;
end;

function TGLExtensionsAndEntryPoints.GetAddress(ProcName: string; glCap: Pointer): Pointer;
var
  vName: string;
begin
  vName := glPrefix + ProcName;
  Result := GLGetProcAddress(PGLChar(TGLString(vName)), nil);
  {$IFDEF DARWIN}
  if Result = nil then
  begin
    Result := AGLGetProcAddress(PGLChar(TGLString(vName)));
    if Result = nil then
    begin
      vName := glPrefix + ProcName + 'APPLE';
      Result := GLGetProcAddress(PGLChar(TGLString(vName)));
  {$ENDIF}
  if Result = nil then
  begin
    vName := glPrefix + ProcName + 'ARB';
    Result := GLGetProcAddress(PGLChar(TGLString(vName)), nil);
    if Result = nil then
    begin
      vName := glPrefix + ProcName;
      Result := GLLibGetProcAddress(PGLChar(TGLString(vName)));
      if Result = nil then
      begin
        vName := glPrefix + ProcName + 'EXT';
        Result := GLGetProcAddress(PGLChar(TGLString(vName)), nil);
        if Result = nil then
        begin
          vName := glPrefix + ProcName + 'NV';
          Result := GLGetProcAddress(PGLChar(TGLString(vName)), nil);
          if Result = nil then
          begin
            vName := glPrefix + ProcName + 'ATI';
            Result := GLGetProcAddress(PGLChar(TGLString(vName)), nil);
            if Result = nil then
            begin
              vName := glPrefix + ProcName + 'OES';
              Result := GLGetProcAddress(PGLChar(TGLString(vName)), nil);
              if Result = nil then
                Result := glCap;
            end;
          end;
        end;
      end;
    end;
  end;
  {$IFDEF DARWIN}
    end;
  end;
  {$ENDIF}
{$IFDEF GLS_OPENGL_DEBUG}
  if Result <> glCap then
    GLSLogger.LogDebug('Finded entry point of ' + vName)
  else
    begin
    GLSLogger.LogDebug('Can''t find entry point of '+vName);
    end;
{$ENDIF}
end;

function TGLExtensionsAndEntryPoints.GetAddressAlt(ProcName1, ProcName2:
  string; glCap: Pointer): Pointer;
begin
  Result := GetAddress(ProcName1, glCap);
  if Result = glCap then
    Result := GetAddress(ProcName2, glCap);
end;

function TGLExtensionsAndEntryPoints.GetAddressNoSuffixes(ProcName: string;
  glCap: Pointer): Pointer;
var
  vName: string;
begin
  vName := glPrefix + ProcName;
  Result := GLGetProcAddress(PGLChar(TGLString(vName)), nil);
  if Result = nil then
    Result := glCap;
{$IFDEF GLS_OPENGL_DEBUG}
  if Result <> glCap then
    GLSLogger.LogDebug('Finded entry point of ' + vName)
  else
    GLSLogger.LogDebug('Can''t find entry point of ' + vName);
{$ENDIF}
end;

procedure TGLExtensionsAndEntryPoints.CheckError;
var
  glError: TGLuint;
  Count: word;
begin
  if FInitialized then
    try
      glError := GetError();
      if glError <> GL_NO_ERROR then
      begin
        Count := 0;
        try
          while (GetError <> GL_NO_ERROR) and (Count < 6) do
            Inc(Count);
        except
        end;
        if not (FDebug and ARB_debug_output) then
          case glError of
            GL_INVALID_ENUM:
              GLSLogger.LogError(format(rstrOpenGLError, ['Invalid enum']));
            GL_INVALID_VALUE:
              GLSLogger.LogError(format(rstrOpenGLError, ['Invalid value']));
            GL_INVALID_OPERATION:
              GLSLogger.LogError(format(rstrOpenGLError, ['Invalid Operation']));
            GL_OUT_OF_MEMORY:
              GLSLogger.LogError(format(rstrOpenGLError, ['Out of memory']));
          end;
      end;
    except
      GLSLogger.LogError(format(rstrOpenGLError, ['Exception in glGetError']));
    end;
end;

procedure TGLExtensionsAndEntryPoints.ClearError;
var
  n: integer;
begin
  n := 0;
  while (GetError <> GL_NO_ERROR) and (n < 6) do
    Inc(n);
end;

function TGLExtensionsAndEntryPoints.CheckExtension(const Extension: string): boolean;
var
  ExtPos: integer;
begin
  // First find the position of the extension string as substring in Buffer.
  ExtPos := Pos(Extension, FBuffer);
  Result := ExtPos > 0;
  // Now check that it isn't only a substring of another extension.
  if Result then
    Result := ((ExtPos + length(Extension) - 1) = length(FBuffer)) or
      (FBuffer[ExtPos + length(Extension)] = ' ');
{$IFDEF GLS_OPENGL_DEBUG}
  if Result then
    GLSLogger.LogDebug(Extension);
{$ENDIF}
end;

procedure TGLExtensionsAndEntryPoints.Initialize(ATemporary: boolean);
var
  i: integer;
  numExt: TGLint;
  MajorVersion, MinorVersion: integer;
begin
  GLSLogger.LogNotice('Getting OpenGL entry points and extension');
{$IFDEF SUPPORT_WGL}
  ReadWGLExtensions;
  ReadWGLImplementationProperties;
{$ENDIF}
{$IFDEF SUPPORT_GLX}
  ReadGLXExtensions;
  ReadGLXImplementationProperties;
{$ENDIF}
{$IFDEF DARWIN}
  ReadAGLExtensions;
  ReadAGLImplementationProperties;
{$ENDIF}
  GetString := GetAddress('GetString',@glCapGetString);
  GetStringi := GetAddress('GetStringi',@glCapGetStringi);
  GetIntegerv := GetAddress('GetIntegerv',@glCapGetIntegerv);
  GetError := GetAddress('GetError',@glCapGetError);
  // determine OpenGL versions supported
  FBuffer := string(GetString(GL_VERSION));
  TrimAndSplitVersionString(FBuffer, MajorVersion, MinorVersion);
  VERSION_1_0 := True;
  VERSION_1_1 := IsVersionMet(1, 1, MajorVersion, MinorVersion);
  VERSION_1_2 := IsVersionMet(1, 2, MajorVersion, MinorVersion);
  VERSION_1_3 := IsVersionMet(1, 3, MajorVersion, MinorVersion);
  VERSION_1_4 := IsVersionMet(1, 4, MajorVersion, MinorVersion);
  VERSION_1_5 := IsVersionMet(1, 5, MajorVersion, MinorVersion);
  VERSION_2_0 := IsVersionMet(2, 0, MajorVersion, MinorVersion);
  VERSION_2_1 := IsVersionMet(2, 1, MajorVersion, MinorVersion);
  VERSION_3_0 := IsVersionMet(3, 0, MajorVersion, MinorVersion);
  VERSION_3_1 := IsVersionMet(3, 1, MajorVersion, MinorVersion);
  VERSION_3_2 := IsVersionMet(3, 2, MajorVersion, MinorVersion);
  VERSION_3_3 := IsVersionMet(3, 3, MajorVersion, MinorVersion);
  VERSION_4_0 := IsVersionMet(4, 0, MajorVersion, MinorVersion);
  VERSION_4_1 := IsVersionMet(4, 1, MajorVersion, MinorVersion);
  VERSION_4_2 := IsVersionMet(4, 2, MajorVersion, MinorVersion);

  if vNotInformed then
  begin
    GLSLogger.LogInfo(format('Platform     : %s', [GLScene_Platform.GetPlatformVersionStr]));
    GLSLogger.LogNotice('');
    GLSLogger.LogInfo('OpenGL rendering context information:');
    GLSLogger.LogInfo(format('Renderer     : %s', [GetString(GL_RENDERER)]));
    GLSLogger.LogInfo(format('Vendor       : %s', [GetString(GL_VENDOR)]));
    GLSLogger.LogInfo(format('Version      : %s', [GetString(GL_VERSION)]));
    if VERSION_2_0 then
      GLSLogger.LogInfo(format('GLSL version : %s',
        [GetString(GL_SHADING_LANGUAGE_VERSION)]))
    else
      GLSLogger.LogWarning('GLSL version : not supported');
    GLSLogger.LogNotice('');
    vNotInformed := False;
  end;

  if ATemporary then
  begin
    FInitialized := True;
    exit;
  end;

  // check supported OpenGL extensions
  if VERSION_3_0 then
  begin
    FBuffer := '';
    GetIntegerv(GL_NUM_EXTENSIONS, @numExt);
    for i := 0 to numExt - 1 do
      FBuffer := FBuffer + string(GetStringi(GL_EXTENSIONS, i)) + ' ';
  end
  else
    FBuffer := string(GetString(GL_EXTENSIONS));
  // check ARB approved OpenGL extensions
  ARB_blend_func_extended := CheckExtension('GL_ARB_blend_func_extended');
  ARB_color_buffer_float := CheckExtension('GL_ARB_color_buffer_float');
  ARB_compatibility := CheckExtension('GL_ARB_compatibility');
  ARB_copy_buffer := CheckExtension('GL_ARB_copy_buffer');
  ARB_depth_buffer_float := CheckExtension('GL_ARB_depth_buffer_float');
  ARB_depth_clamp := CheckExtension('GL_ARB_depth_clamp');
  ARB_depth_texture := CheckExtension('GL_ARB_depth_texture');
  ARB_draw_buffers := CheckExtension('GL_ARB_draw_buffers');
  ARB_draw_buffers_blend := CheckExtension('GL_ARB_draw_buffers_blend');
  ARB_draw_elements_base_vertex := CheckExtension('GL_ARB_draw_elements_base_vertex');
  ARB_draw_indirect := CheckExtension('GL_ARB_draw_indirect');
  ARB_draw_instanced := CheckExtension('GL_ARB_draw_instanced');
  ARB_explicit_attrib_location := CheckExtension('GL_ARB_explicit_attrib_location');
  ARB_fragment_coord_conventions := CheckExtension('GL_ARB_fragment_coord_conventions');
  ARB_fragment_program := CheckExtension('GL_ARB_fragment_program');
  ARB_fragment_program_shadow := CheckExtension('GL_ARB_fragment_program_shadow');
  ARB_fragment_shader := CheckExtension('GL_ARB_fragment_shader');
  ARB_framebuffer_object := CheckExtension('GL_ARB_framebuffer_object');
  ARB_framebuffer_sRGB := CheckExtension('GL_ARB_framebuffer_sRGB');
  ARB_geometry_shader4 := CheckExtension('GL_ARB_geometry_shader4');
  ARB_gpu_shader_fp64 := CheckExtension('GL_ARB_gpu_shader_fp64');
  ARB_gpu_shader5 := CheckExtension('GL_ARB_gpu_shader5');
  ARB_half_float_pixel := CheckExtension('GL_ARB_half_float_pixel');
  ARB_half_float_vertex := CheckExtension('GL_ARB_half_float_vertex');
  ARB_imaging := CheckExtension('GL_ARB_imaging');
  ARB_instanced_arrays := CheckExtension('GL_ARB_instanced_arrays');
  ARB_map_buffer_range := CheckExtension('GL_ARB_map_buffer_range');
  ARB_matrix_palette := CheckExtension('GL_ARB_matrix_palette');
  ARB_multisample := CheckExtension('GL_ARB_multisample');
  // ' ' to avoid collision with WGL variant
  ARB_multitexture := CheckExtension('GL_ARB_multitexture');
  ARB_occlusion_query := CheckExtension('GL_ARB_occlusion_query');
  ARB_occlusion_query2 := CheckExtension('GL_ARB_occlusion_query2');
  ARB_pixel_buffer_object := CheckExtension('GL_ARB_pixel_buffer_object');
  ARB_point_parameters := CheckExtension('GL_ARB_point_parameters');
  ARB_point_sprite := CheckExtension('GL_ARB_point_sprite');
  ARB_provoking_vertex := CheckExtension('GL_ARB_provoking_vertex');
  ARB_sample_shading := CheckExtension('GL_ARB_sample_shading');
  ARB_sampler_objects := CheckExtension('GL_ARB_sampler_objects');
  ARB_seamless_cube_map := CheckExtension('GL_ARB_seamless_cube_map');
  ARB_shader_bit_encoding := CheckExtension('GL_ARB_shader_bit_encoding');
  ARB_shader_objects := CheckExtension('GL_ARB_shader_objects');
  ARB_shader_subroutine := CheckExtension('GL_ARB_shader_subroutine');
  ARB_shader_texture_lod := CheckExtension('GL_ARB_shader_texture_lod');
  ARB_shading_language_100 := CheckExtension('GL_ARB_shading_language_100');
  ARB_shadow := CheckExtension('GL_ARB_shadow');
  ARB_shadow_ambient := CheckExtension('GL_ARB_shadow_ambient');
  ARB_sync := CheckExtension('GL_ARB_sync');
  ARB_tessellation_shader := CheckExtension('GL_ARB_tessellation_shader');
  ARB_texture_border_clamp := CheckExtension('GL_ARB_texture_border_clamp');
  ARB_texture_buffer_object := CheckExtension('GL_ARB_texture_buffer_object');
  ARB_texture_buffer_object_rgb32 :=
    CheckExtension('GL_ARB_texture_buffer_object_rgb32');
  ARB_texture_compression := CheckExtension('GL_ARB_texture_compression');
  ARB_texture_compression_rgtc := CheckExtension('GL_ARB_texture_compression_rgtc');
  ARB_texture_cube_map := CheckExtension('GL_ARB_texture_cube_map');
  ARB_texture_cube_map_array := CheckExtension('GL_ARB_texture_cube_map_array');
  ARB_texture_env_add := CheckExtension('GL_ARB_texture_env_add');
  ARB_texture_env_combine := CheckExtension('GL_ARB_texture_env_combine');
  ARB_texture_env_crossbar := CheckExtension('GL_ARB_texture_env_crossbar');
  ARB_texture_env_dot3 := CheckExtension('GL_ARB_texture_env_dot3');
  ARB_texture_float := CheckExtension('GL_ARB_texture_float');
  ARB_texture_gather := CheckExtension('GL_ARB_texture_gather');
  ARB_texture_mirrored_repeat := CheckExtension('GL_ARB_texture_mirrored_repeat');
  ARB_texture_multisample := CheckExtension('GL_ARB_texture_multisample');
  ARB_texture_non_power_of_two := CheckExtension('GL_ARB_texture_non_power_of_two');
  ARB_texture_query_lod := CheckExtension('GL_ARB_texture_query_lod');
  ARB_texture_rectangle := CheckExtension('GL_ARB_texture_rectangle');
  ARB_texture_rg := CheckExtension('GL_ARB_texture_rg');
  ARB_texture_rgb10_a2ui := CheckExtension('GL_ARB_texture_rgb10_a2ui');
  ARB_texture_swizzle := CheckExtension('GL_ARB_texture_swizzle');
  ARB_timer_query := CheckExtension('GL_ARB_timer_query');
  ARB_transform_feedback2 := CheckExtension('GL_ARB_transform_feedback2');
  ARB_transform_feedback3 := CheckExtension('GL_ARB_transform_feedback3');
  ARB_transpose_matrix := CheckExtension('GL_ARB_transpose_matrix');
  ARB_uniform_buffer_object := CheckExtension('GL_ARB_uniform_buffer_object');
  ARB_vertex_array_bgra := CheckExtension('GL_ARB_vertex_array_bgra');
  ARB_vertex_array_object := CheckExtension('GL_ARB_vertex_array_object');
  ARB_vertex_blend := CheckExtension('GL_ARB_vertex_blend');
  ARB_vertex_buffer_object := CheckExtension('GL_ARB_vertex_buffer_object');
  ARB_vertex_program := CheckExtension('GL_ARB_vertex_program');
  ARB_vertex_shader := CheckExtension('GL_ARB_vertex_shader');
  ARB_vertex_type_2_10_10_10_rev := CheckExtension('GL_ARB_vertex_type_2_10_10_10_rev');
  ARB_window_pos := CheckExtension('GL_ARB_window_pos');
  ARB_texture_compression_bptc := CheckExtension('GL_ARB_texture_compression_bptc');
  ARB_get_program_binary := CheckExtension('GL_ARB_get_program_binary');
  ARB_separate_shader_objects := CheckExtension('GL_ARB_separate_shader_objects');

  // check Vendor/EXT OpenGL extensions
  _3DFX_multisample := CheckExtension('GL_3DFX_multisample');
  _3DFX_tbuffer := CheckExtension('GL_3DFX_tbuffer');
  _3DFX_texture_compression_FXT1 := CheckExtension('GL_3DFX_texture_compression_FXT1');
  ATI_draw_buffers := CheckExtension('GL_ATI_draw_buffers');
  ATI_texture_compression_3dc := CheckExtension('GL_ATI_texture_compression_3dc');
  ATI_texture_float := CheckExtension('GL_ATI_texture_float');
  ATI_texture_mirror_once := CheckExtension('GL_ATI_texture_mirror_once');

  S3_s3tc := CheckExtension('GL_S3_s3tc');

  EXT_abgr := CheckExtension('GL_EXT_abgr');
  EXT_bgra := CheckExtension('GL_EXT_bgra');
  EXT_bindable_uniform := CheckExtension('GL_EXT_bindable_uniform');
  EXT_blend_color := CheckExtension('GL_EXT_blend_color');
  EXT_blend_equation_separate := CheckExtension('GL_EXT_blend_equation_separate');
  EXT_blend_func_separate := CheckExtension('GL_EXT_blend_func_separate');
  EXT_blend_logic_op := CheckExtension('GL_EXT_blend_logic_op');
  EXT_blend_minmax := CheckExtension('GL_EXT_blend_minmax');
  EXT_blend_subtract := CheckExtension('GL_EXT_blend_subtract');
  EXT_Cg_shader := CheckExtension('GL_EXT_Cg_shader');
  EXT_clip_volume_hint := CheckExtension('GL_EXT_clip_volume_hint');
  EXT_compiled_vertex_array := CheckExtension('GL_EXT_compiled_vertex_array');
  EXT_copy_texture := CheckExtension('GL_EXT_copy_texture');
  EXT_depth_bounds_test := CheckExtension('GL_EXT_depth_bounds_test');
  EXT_draw_buffers2 := CheckExtension('GL_EXT_draw_buffers2');
  EXT_draw_instanced := CheckExtension('GL_EXT_draw_instanced');
  EXT_draw_range_elements := CheckExtension('GL_EXT_draw_range_elements');
  EXT_fog_coord := CheckExtension('GL_EXT_fog_coord');
  EXT_framebuffer_blit := CheckExtension('GL_EXT_framebuffer_blit');
  EXT_framebuffer_multisample := CheckExtension('GL_EXT_framebuffer_multisample');
  EXT_framebuffer_object := CheckExtension('GL_EXT_framebuffer_object');
  EXT_framebuffer_sRGB := CheckExtension('GL_EXT_framebuffer_sRGB');
  EXT_geometry_shader4 := CheckExtension('GL_EXT_geometry_shader4');
  EXT_gpu_program_parameters := CheckExtension('GL_EXT_gpu_program_parameters');
  EXT_gpu_shader4 := CheckExtension('GL_EXT_gpu_shader4');
  EXT_multi_draw_arrays := CheckExtension('GL_EXT_multi_draw_arrays');
  EXT_multisample := CheckExtension('GL_EXT_multisample');
  EXT_packed_depth_stencil := CheckExtension('GL_EXT_packed_depth_stencil');
  EXT_packed_float := CheckExtension('GL_EXT_packed_float');
  EXT_packed_pixels := CheckExtension('GL_EXT_packed_pixels');
  EXT_paletted_texture := CheckExtension('GL_EXT_paletted_texture');
  EXT_pixel_buffer_object := CheckExtension('GL_EXT_pixel_buffer_object');
  EXT_polygon_offset := CheckExtension('GL_EXT_polygon_offset');
  EXT_rescale_normal := CheckExtension('GL_EXT_rescale_normal');
  EXT_secondary_color := CheckExtension('GL_EXT_secondary_color');
  EXT_separate_specular_color := CheckExtension('GL_EXT_separate_specular_color');
  EXT_shadow_funcs := CheckExtension('GL_EXT_shadow_funcs');
  EXT_shared_texture_palette := CheckExtension('GL_EXT_shared_texture_palette');
  EXT_stencil_clear_tag := CheckExtension('GL_EXT_stencil_clear_tag');
  EXT_stencil_two_side := CheckExtension('GL_EXT_stencil_two_side');
  EXT_stencil_wrap := CheckExtension('GL_EXT_stencil_wrap');
  EXT_texture3D := CheckExtension('GL_EXT_texture3D');
  EXT_texture_array := CheckExtension('GL_EXT_texture_array');
  EXT_texture_buffer_object := CheckExtension('GL_EXT_texture_buffer_object');
  EXT_texture_compression_latc := CheckExtension('GL_EXT_texture_compression_latc');
  EXT_texture_compression_rgtc := CheckExtension('GL_EXT_texture_compression_rgtc');
  EXT_texture_compression_s3tc := CheckExtension('GL_EXT_texture_compression_s3tc');
  EXT_texture_cube_map := CheckExtension('GL_EXT_texture_cube_map');
  EXT_texture_edge_clamp := CheckExtension('GL_EXT_texture_edge_clamp');
  EXT_texture_env_add := CheckExtension('GL_EXT_texture_env_add');
  EXT_texture_env_combine := CheckExtension('GL_EXT_texture_env_combine');
  EXT_texture_env_dot3 := CheckExtension('GL_EXT_texture_env_dot3');
  EXT_texture_filter_anisotropic := CheckExtension('GL_EXT_texture_filter_anisotropic');
  EXT_texture_integer := CheckExtension('GL_EXT_texture_integer');
  EXT_texture_lod := CheckExtension('GL_EXT_texture_lod');
  EXT_texture_lod_bias := CheckExtension('GL_EXT_texture_lod_bias');
  EXT_texture_mirror_clamp := CheckExtension('GL_EXT_texture_mirror_clamp');
  EXT_texture_object := CheckExtension('GL_EXT_texture_object');
  EXT_texture_rectangle := CheckExtension('GL_EXT_texture_rectangle');
  EXT_texture_sRGB := CheckExtension('GL_EXT_texture_sRGB');
  EXT_texture_shared_exponent := CheckExtension('GL_EXT_texture_shared_exponent');
  EXT_timer_query := CheckExtension('GL_EXT_timer_query');
  EXT_transform_feedback := CheckExtension('GL_EXT_transform_feedback');
  EXT_vertex_array := CheckExtension('GL_EXT_vertex_array');
  EXT_texture_sRGB_decode := CheckExtension('GL_EXT_texture_sRGB_decode');
  EXT_direct_state_access := CheckExtension('EXT_direct_state_access');
  EXT_texture_swizzle := CheckExtension('EXT_texture_swizzle');

  HP_occlusion_test := CheckExtension('GL_HP_occlusion_test');

  IBM_rasterpos_clip := CheckExtension('GL_IBM_rasterpos_clip');

  KTX_buffer_region := CheckExtension('GL_KTX_buffer_region');

  MESA_resize_buffers := CheckExtension('GL_MESA_resize_buffers');

  NV_blend_square := CheckExtension('GL_NV_blend_square');
  NV_conditional_render := CheckExtension('GL_NV_conditional_render');
  NV_copy_image := CheckExtension('GL_NV_copy_image');
  NV_depth_buffer_float := CheckExtension('GL_NV_depth_buffer_float');
  NV_fence := CheckExtension('GL_NV_fence');
  NV_float_buffer := CheckExtension('GL_NV_float_buffer');
  NV_fog_distance := CheckExtension('GL_NV_fog_distance');
  NV_geometry_program4 := CheckExtension('GL_NV_geometry_program4');
  NV_light_max_exponent := CheckExtension('GL_NV_light_max_exponent');
  NV_multisample_filter_hint := CheckExtension('GL_NV_multisample_filter_hint');
  NV_occlusion_query := CheckExtension('GL_NV_occlusion_query');
  NV_point_sprite := CheckExtension('GL_NV_point_sprite');
  NV_primitive_restart := CheckExtension('GL_NV_primitive_restart');
  NV_register_combiners := CheckExtension('GL_NV_register_combiners');
  NV_shader_buffer_load := CheckExtension('GL_NV_shader_buffer_load');
  NV_texgen_reflection := CheckExtension('GL_NV_texgen_reflection');
  NV_texture_compression_vtc := CheckExtension('GL_NV_texture_compression_vtc');
  NV_texture_env_combine4 := CheckExtension('GL_NV_texture_env_combine4');
  NV_texture_rectangle := CheckExtension('GL_NV_texture_rectangle');
  NV_texture_shader := CheckExtension('GL_NV_texture_shader');
  NV_texture_shader2 := CheckExtension('GL_NV_texture_shader2');
  NV_texture_shader3 := CheckExtension('GL_NV_texture_shader3');
  NV_transform_feedback := CheckExtension('GL_NV_transform_feedback');
  NV_vertex_array_range := CheckExtension('GL_NV_vertex_array_range');
  NV_vertex_array_range2 := CheckExtension('GL_NV_vertex_array_range2');
  NV_vertex_buffer_unified_memory :=
    CheckExtension('GL_NV_vertex_buffer_unified_memory');
  NV_vertex_program := CheckExtension('GL_NV_vertex_program');

  SGI_color_matrix := CheckExtension('GL_SGI_color_matrix');

  SGIS_generate_mipmap := CheckExtension('GL_SGIS_generate_mipmap');
  SGIS_multisample := CheckExtension('GL_SGIS_multisample');
  SGIS_texture_border_clamp := CheckExtension('GL_SGIS_texture_border_clamp');
  SGIS_texture_color_mask := CheckExtension('GL_SGIS_texture_color_mask');
  SGIS_texture_edge_clamp := CheckExtension('GL_SGIS_texture_edge_clamp');
  SGIS_texture_lod := CheckExtension('GL_SGIS_texture_lod');

  SGIX_depth_texture := CheckExtension('GL_SGIX_depth_texture');
  SGIX_shadow := CheckExtension('GL_SGIX_shadow');
  SGIX_shadow_ambient := CheckExtension('GL_SGIX_shadow_ambient');

  AMD_vertex_shader_tessellator := CheckExtension('GL_AMD_vertex_shader_tessellator');

  WIN_swap_hint := CheckExtension('GL_WIN_swap_hint');
  ATI_meminfo := CheckExtension('GL_ATI_meminfo');
  NVX_gpu_memory_info := CheckExtension('GL_NVX_gpu_memory_info');
  NV_vdpau_interop := CheckExtension('GL_NV_vdpau_interop');
  NV_path_rendering := CheckExtension('GL_NV_path_rendering');

  GREMEDY_frame_terminator := CheckExtension('GL_GREMEDY_frame_terminator');
  GREMEDY_string_marker := CheckExtension('GL_GREMEDY_string_marker');
  AMDX_debug_output := CheckExtension('AMDX_debug_output');
  ARB_debug_output := CheckExtension('GL_ARB_debug_output');

{$IFDEF GLS_OPENGL_ES}
  OES_depth24 := CheckExtension('GL_OES_depth24');
  OES_depth32 := CheckExtension('GL_OES_depth32');
  OES_depth_texture := CheckExtension('GL_OES_depth_texture');
  OES_element_index_uint := CheckExtension('GL_OES_element_index_uint');
  OES_fbo_render_mipmap := CheckExtension('GL_OES_fbo_render_mipmap');
  OES_get_program_binary := CheckExtension('GL_OES_get_program_binary');
  OES_mapbuffer := CheckExtension('GL_OES_mapbuffer');
  OES_packed_depth_stencil := CheckExtension('GL_OES_packed_depth_stencil');
  OES_rgb8_rgba8 := CheckExtension('GL_OES_rgb8_rgba8');
  OES_standard_derivatives := CheckExtension('GL_OES_standard_derivatives');
  OES_texture_3D := CheckExtension('GL_OES_texture_3D');
  OES_texture_float := CheckExtension('GL_OES_texture_float');
  OES_texture_float_linear := CheckExtension('GL_OES_texture_float_linear');
  OES_texture_half_float := CheckExtension('GL_OES_texture_half_float');
  OES_texture_half_float_linear := CheckExtension('GL_OES_texture_half_float_linear');
  OES_texture_npot := CheckExtension('GL_OES_texture_npot');
  OES_vertex_array_object := CheckExtension('GL_OES_vertex_array_object');
  OES_vertex_half_float := CheckExtension('GL_OES_vertex_half_float');
  EXT_texture_format_BGRA8888 := CheckExtension('GL_EXT_texture_format_BGRA8888');
  EXT_read_format_bgra := CheckExtension('GL_EXT_read_format_bgra');
  ANGLE_framebuffer_blit := CheckExtension('GL_ANGLE_framebuffer_blit');
  EXT_texture_compression_dxt1 := CheckExtension('GL_EXT_texture_compression_dxt1');
  ANGLE_framebuffer_multisample := CheckExtension('GL_ANGLE_framebuffer_multisample');
{$ENDIF}

  BindTexture := GetAddress('BindTexture',@glCapBindTexture);
  BlendFunc := GetAddress('BlendFunc',@glCapBlendFunc);
  Clear := GetAddress('Clear',@glCapClear);
  ClearColor := GetAddress('ClearColor',@glCapClearColor);
  ClearDepth := GetAddress('ClearDepth',@glCapClearDepth);
  ClearStencil := GetAddress('ClearStencil',@glCapClearStencil);
  ColorMask := GetAddress('ColorMask',@glCapColorMask);
  CopyTexImage1D := GetAddress('CopyTexImage1D',@glCapCopyTexImage1D);
  CopyTexImage2D := GetAddress('CopyTexImage2D',@glCapCopyTexImage2D);
  CopyTexSubImage1D := GetAddress('CopyTexSubImage1D',@glCapCopyTexSubImage1D);
  CopyTexSubImage2D := GetAddress('CopyTexSubImage2D',@glCapCopyTexSubImage2D);
  CullFace := GetAddress('CullFace',@glCapCullFace);
  DeleteTextures := GetAddress('DeleteTextures',@glCapDeleteTextures);
  DepthFunc := GetAddress('DepthFunc',@glCapDepthFunc);
  DepthMask := GetAddress('DepthMask',@glCapDepthMask);
  DepthRange := GetAddress('DepthRange',@glCapDepthRange);
  Disable := GetAddress('Disable',@glCapDisable);
  DrawArrays := GetAddress('DrawArrays',@glCapDrawArrays);
  DrawBuffer := GetAddress('DrawBuffer',@glCapDrawBuffer);
  DrawElements := GetAddress('DrawElements',@glCapDrawElements);
  Enable := GetAddress('Enable',@glCapEnable);
  Finish := GetAddress('Finish',@glCapFinish);
  Flush := GetAddress('Flush',@glCapFlush);
  FrontFace := GetAddress('FrontFace',@glCapFrontFace);
  GenTextures := GetAddress('GenTextures',@glCapGenTextures);
  GetBooleanv := GetAddress('GetBooleanv',@glCapGetBooleanv);
  GetDoublev := GetAddress('GetDoublev',@glCapGetDoublev);
  GetFloatv := GetAddress('GetFloatv',@glCapGetFloatv);
  GetPointerv := GetAddress('GetPointerv',@glCapGetPointerv);
  GetString := GetAddress('GetString',@glCapGetString);
  GetTexImage := GetAddress('GetTexImage',@glCapGetTexImage);
  GetTexLevelParameterfv := GetAddress('GetTexLevelParameterfv',@glCapGetTexLevelParameterfv);
  GetTexLevelParameteriv := GetAddress('GetTexLevelParameteriv',@glCapGetTexLevelParameteriv);
  GetTexParameterfv := GetAddress('GetTexParameterfv',@glCapGetTexParameterfv);
  GetTexParameteriv := GetAddress('GetTexParameteriv',@glCapGetTexParameteriv);
  Hint := GetAddress('Hint',@glCapHint);
  IsEnabled := GetAddress('IsEnabled',@glCapIsEnabled);
  IsTexture := GetAddress('IsTexture',@glCapIsTexture);
  LineWidth := GetAddress('LineWidth',@glCapLineWidth);
  LogicOp := GetAddress('LogicOp',@glCapLogicOp);
  PixelStoref := GetAddress('PixelStoref',@glCapPixelStoref);
  PixelStorei := GetAddress('PixelStorei',@glCapPixelStorei);
  PointSize := GetAddress('PointSize',@glCapPointSize);
  PolygonMode := GetAddress('PolygonMode',@glCapPolygonMode);
  PolygonOffset := GetAddress('PolygonOffset',@glCapPolygonOffset);
  ReadBuffer := GetAddress('ReadBuffer',@glCapReadBuffer);
  ReadPixels := GetAddress('ReadPixels',@glCapReadPixels);
  Scissor := GetAddress('Scissor',@glCapScissor);
  StencilFunc := GetAddress('StencilFunc',@glCapStencilFunc);
  StencilMask := GetAddress('StencilMask',@glCapStencilMask);
  StencilOp := GetAddress('StencilOp',@glCapStencilOp);
  TexImage1D := GetAddress('TexImage1D',@glCapTexImage1D);
  TexImage2D := GetAddress('TexImage2D',@glCapTexImage2D);
  TexParameterf := GetAddress('TexParameterf',@glCapTexParameterf);
  TexParameterfv := GetAddress('TexParameterfv',@glCapTexParameterfv);
  TexParameteri := GetAddress('TexParameteri',@glCapTexParameteri);
  TexParameteriv := GetAddress('TexParameteriv',@glCapTexParameteriv);
  TexSubImage1D := GetAddress('TexSubImage1D',@glCapTexSubImage1D);
  TexSubImage2D := GetAddress('TexSubImage2D',@glCapTexSubImage2D);
  Viewport := GetAddress('Viewport',@glCapViewport);
  Accum := GetAddress('Accum',@glCapAccum);
  AlphaFunc := GetAddress('AlphaFunc',@glCapAlphaFunc);
  AreTexturesResident := GetAddress('AreTexturesResident',@glCapAreTexturesResident);
  ArrayElement := GetAddress('ArrayElement',@glCapArrayElement);
  Begin_ := GetAddress('Begin',@glCapBegin_);
  Bitmap := GetAddress('Bitmap',@glCapBitmap);
  CallList := GetAddress('CallList',@glCapCallList);
  CallLists := GetAddress('CallLists',@glCapCallLists);
  ClearAccum := GetAddress('ClearAccum',@glCapClearAccum);
  ClearIndex := GetAddress('ClearIndex',@glCapClearIndex);
  ClipPlane := GetAddress('ClipPlane',@glCapClipPlane);
  Color3b := GetAddress('Color3b',@glCapColor3b);
  Color3bv := GetAddress('Color3bv',@glCapColor3bv);
  Color3d := GetAddress('Color3d',@glCapColor3d);
  Color3dv := GetAddress('Color3dv',@glCapColor3dv);
  Color3f := GetAddress('Color3f',@glCapColor3f);
  Color3fv := GetAddress('Color3fv',@glCapColor3fv);
  Color3i := GetAddress('Color3i',@glCapColor3i);
  Color3iv := GetAddress('Color3iv',@glCapColor3iv);
  Color3s := GetAddress('Color3s',@glCapColor3s);
  Color3sv := GetAddress('Color3sv',@glCapColor3sv);
  Color3ub := GetAddress('Color3ub',@glCapColor3ub);
  Color3ubv := GetAddress('Color3ubv',@glCapColor3ubv);
  Color3ui := GetAddress('Color3ui',@glCapColor3ui);
  Color3uiv := GetAddress('Color3uiv',@glCapColor3uiv);
  Color3us := GetAddress('Color3us',@glCapColor3us);
  Color3usv := GetAddress('Color3usv',@glCapColor3usv);
  Color4b := GetAddress('Color4b',@glCapColor4b);
  Color4bv := GetAddress('Color4bv',@glCapColor4bv);
  Color4d := GetAddress('Color4d',@glCapColor4d);
  Color4dv := GetAddress('Color4dv',@glCapColor4dv);
  Color4f := GetAddress('Color4f',@glCapColor4f);
  Color4fv := GetAddress('Color4fv',@glCapColor4fv);
  Color4i := GetAddress('Color4i',@glCapColor4i);
  Color4iv := GetAddress('Color4iv',@glCapColor4iv);
  Color4s := GetAddress('Color4s',@glCapColor4s);
  Color4sv := GetAddress('Color4sv',@glCapColor4sv);
  Color4ub := GetAddress('Color4ub',@glCapColor4ub);
  Color4ubv := GetAddress('Color4ubv',@glCapColor4ubv);
  Color4ui := GetAddress('Color4ui',@glCapColor4ui);
  Color4uiv := GetAddress('Color4uiv',@glCapColor4uiv);
  Color4us := GetAddress('Color4us',@glCapColor4us);
  Color4usv := GetAddress('Color4usv',@glCapColor4usv);
  ColorMaterial := GetAddress('ColorMaterial',@glCapColorMaterial);
  ColorPointer := GetAddress('ColorPointer',@glCapColorPointer);
  CopyPixels := GetAddress('CopyPixels',@glCapCopyPixels);
  DeleteLists := GetAddress('DeleteLists',@glCapDeleteLists);
  DisableClientState := GetAddress('DisableClientState',@glCapDisableClientState);
  DrawPixels := GetAddress('DrawPixels',@glCapDrawPixels);
  EdgeFlag := GetAddress('EdgeFlag',@glCapEdgeFlag);
  EdgeFlagPointer := GetAddress('EdgeFlagPointer',@glCapEdgeFlagPointer);
  EdgeFlagv := GetAddress('EdgeFlagv',@glCapEdgeFlagv);
  EnableClientState := GetAddress('EnableClientState',@glCapEnableClientState);
  End_ := GetAddress('End',@glCapEnd_);
  EndList := GetAddress('EndList',@glCapEndList);
  EvalCoord1d := GetAddress('EvalCoord1d',@glCapEvalCoord1d);
  EvalCoord1dv := GetAddress('EvalCoord1dv',@glCapEvalCoord1dv);
  EvalCoord1f := GetAddress('EvalCoord1f',@glCapEvalCoord1f);
  EvalCoord1fv := GetAddress('EvalCoord1fv',@glCapEvalCoord1fv);
  EvalCoord2d := GetAddress('EvalCoord2d',@glCapEvalCoord2d);
  EvalCoord2dv := GetAddress('EvalCoord2dv',@glCapEvalCoord2dv);
  EvalCoord2f := GetAddress('EvalCoord2f',@glCapEvalCoord2f);
  EvalCoord2fv := GetAddress('EvalCoord2fv',@glCapEvalCoord2fv);
  EvalMesh1 := GetAddress('EvalMesh1',@glCapEvalMesh1);
  EvalMesh2 := GetAddress('EvalMesh2',@glCapEvalMesh2);
  EvalPoint1 := GetAddress('EvalPoint1',@glCapEvalPoint1);
  EvalPoint2 := GetAddress('EvalPoint2',@glCapEvalPoint2);
  FeedbackBuffer := GetAddress('FeedbackBuffer',@glCapFeedbackBuffer);
  Fogf := GetAddress('Fogf',@glCapFogf);
  Fogfv := GetAddress('Fogfv',@glCapFogfv);
  Fogi := GetAddress('Fogi',@glCapFogi);
  Fogiv := GetAddress('Fogiv',@glCapFogiv);
  Frustum := GetAddress('Frustum',@glCapFrustum);
  GenLists := GetAddress('GenLists',@glCapGenLists);
  GetClipPlane := GetAddress('GetClipPlane',@glCapGetClipPlane);
  GetLightfv := GetAddress('GetLightfv',@glCapGetLightfv);
  GetLightiv := GetAddress('GetLightiv',@glCapGetLightiv);
  GetMapdv := GetAddress('GetMapdv',@glCapGetMapdv);
  GetMapfv := GetAddress('GetMapfv',@glCapGetMapfv);
  GetMapiv := GetAddress('GetMapiv',@glCapGetMapiv);
  GetMaterialfv := GetAddress('GetMaterialfv',@glCapGetMaterialfv);
  GetMaterialiv := GetAddress('GetMaterialiv',@glCapGetMaterialiv);
  GetPixelMapfv := GetAddress('GetPixelMapfv',@glCapGetPixelMapfv);
  GetPixelMapuiv := GetAddress('GetPixelMapuiv',@glCapGetPixelMapuiv);
  GetPixelMapusv := GetAddress('GetPixelMapusv',@glCapGetPixelMapusv);
  GetPolygonStipple := GetAddress('GetPolygonStipple',@glCapGetPolygonStipple);
  GetTexEnvfv := GetAddress('GetTexEnvfv',@glCapGetTexEnvfv);
  GetTexEnviv := GetAddress('GetTexEnviv',@glCapGetTexEnviv);
  GetTexGendv := GetAddress('GetTexGendv',@glCapGetTexGendv);
  GetTexGenfv := GetAddress('GetTexGenfv',@glCapGetTexGenfv);
  GetTexGeniv := GetAddress('GetTexGeniv',@glCapGetTexGeniv);
  IndexMask := GetAddress('IndexMask',@glCapIndexMask);
  IndexPointer := GetAddress('IndexPointer',@glCapIndexPointer);
  Indexd := GetAddress('Indexd',@glCapIndexd);
  Indexdv := GetAddress('Indexdv',@glCapIndexdv);
  Indexf := GetAddress('Indexf',@glCapIndexf);
  Indexfv := GetAddress('Indexfv',@glCapIndexfv);
  Indexi := GetAddress('Indexi',@glCapIndexi);
  Indexiv := GetAddress('Indexiv',@glCapIndexiv);
  Indexs := GetAddress('Indexs',@glCapIndexs);
  Indexsv := GetAddress('Indexsv',@glCapIndexsv);
  Indexub := GetAddress('Indexub',@glCapIndexub);
  Indexubv := GetAddress('Indexubv',@glCapIndexubv);
  InitNames := GetAddress('InitNames',@glCapInitNames);
  InterleavedArrays := GetAddress('InterleavedArrays',@glCapInterleavedArrays);
  IsList := GetAddress('IsList',@glCapIsList);
  LightModelf := GetAddress('LightModelf',@glCapLightModelf);
  LightModelfv := GetAddress('LightModelfv',@glCapLightModelfv);
  LightModeli := GetAddress('LightModeli',@glCapLightModeli);
  LightModeliv := GetAddress('LightModeliv',@glCapLightModeliv);
  Lightf := GetAddress('Lightf',@glCapLightf);
  Lightfv := GetAddress('Lightfv',@glCapLightfv);
  Lighti := GetAddress('Lighti',@glCapLighti);
  Lightiv := GetAddress('Lightiv',@glCapLightiv);
  LineStipple := GetAddress('LineStipple',@glCapLineStipple);
  ListBase := GetAddress('ListBase',@glCapListBase);
  LoadIdentity := GetAddress('LoadIdentity',@glCapLoadIdentity);
  LoadMatrixd := GetAddress('LoadMatrixd',@glCapLoadMatrixd);
  LoadMatrixf := GetAddress('LoadMatrixf',@glCapLoadMatrixf);
  LoadName := GetAddress('LoadName',@glCapLoadName);
  Map1d := GetAddress('Map1d',@glCapMap1d);
  Map1f := GetAddress('Map1f',@glCapMap1f);
  Map2d := GetAddress('Map2d',@glCapMap2d);
  Map2f := GetAddress('Map2f',@glCapMap2f);
  MapGrid1d := GetAddress('MapGrid1d',@glCapMapGrid1d);
  MapGrid1f := GetAddress('MapGrid1f',@glCapMapGrid1f);
  MapGrid2d := GetAddress('MapGrid2d',@glCapMapGrid2d);
  MapGrid2f := GetAddress('MapGrid2f',@glCapMapGrid2f);
  Materialf := GetAddress('Materialf',@glCapMaterialf);
  Materialfv := GetAddress('Materialfv',@glCapMaterialfv);
  Materiali := GetAddress('Materiali',@glCapMateriali);
  Materialiv := GetAddress('Materialiv',@glCapMaterialiv);
  MatrixMode := GetAddress('MatrixMode',@glCapMatrixMode);
  MultMatrixd := GetAddress('MultMatrixd',@glCapMultMatrixd);
  MultMatrixf := GetAddress('MultMatrixf',@glCapMultMatrixf);
  NewList := GetAddress('NewList',@glCapNewList);
  Normal3b := GetAddress('Normal3b',@glCapNormal3b);
  Normal3bv := GetAddress('Normal3bv',@glCapNormal3bv);
  Normal3d := GetAddress('Normal3d',@glCapNormal3d);
  Normal3dv := GetAddress('Normal3dv',@glCapNormal3dv);
  Normal3f := GetAddress('Normal3f',@glCapNormal3f);
  Normal3fv := GetAddress('Normal3fv',@glCapNormal3fv);
  Normal3i := GetAddress('Normal3i',@glCapNormal3i);
  Normal3iv := GetAddress('Normal3iv',@glCapNormal3iv);
  Normal3s := GetAddress('Normal3s',@glCapNormal3s);
  Normal3sv := GetAddress('Normal3sv',@glCapNormal3sv);
  NormalPointer := GetAddress('NormalPointer',@glCapNormalPointer);
  Ortho := GetAddress('Ortho',@glCapOrtho);
  PassThrough := GetAddress('PassThrough',@glCapPassThrough);
  PixelMapfv := GetAddress('PixelMapfv',@glCapPixelMapfv);
  PixelMapuiv := GetAddress('PixelMapuiv',@glCapPixelMapuiv);
  PixelMapusv := GetAddress('PixelMapusv',@glCapPixelMapusv);
  PixelTransferf := GetAddress('PixelTransferf',@glCapPixelTransferf);
  PixelTransferi := GetAddress('PixelTransferi',@glCapPixelTransferi);
  PixelZoom := GetAddress('PixelZoom',@glCapPixelZoom);
  PolygonStipple := GetAddress('PolygonStipple',@glCapPolygonStipple);
  PopAttrib := GetAddress('PopAttrib',@glCapPopAttrib);
  PopClientAttrib := GetAddress('PopClientAttrib',@glCapPopClientAttrib);
  PopMatrix := GetAddress('PopMatrix',@glCapPopMatrix);
  PopName := GetAddress('PopName',@glCapPopName);
  PrioritizeTextures := GetAddress('PrioritizeTextures',@glCapPrioritizeTextures);
  PushAttrib := GetAddress('PushAttrib',@glCapPushAttrib);
  PushClientAttrib := GetAddress('PushClientAttrib',@glCapPushClientAttrib);
  PushMatrix := GetAddress('PushMatrix',@glCapPushMatrix);
  PushName := GetAddress('PushName',@glCapPushName);
  RasterPos2d := GetAddress('RasterPos2d',@glCapRasterPos2d);
  RasterPos2dv := GetAddress('RasterPos2dv',@glCapRasterPos2dv);
  RasterPos2f := GetAddress('RasterPos2f',@glCapRasterPos2f);
  RasterPos2fv := GetAddress('RasterPos2fv',@glCapRasterPos2fv);
  RasterPos2i := GetAddress('RasterPos2i',@glCapRasterPos2i);
  RasterPos2iv := GetAddress('RasterPos2iv',@glCapRasterPos2iv);
  RasterPos2s := GetAddress('RasterPos2s',@glCapRasterPos2s);
  RasterPos2sv := GetAddress('RasterPos2sv',@glCapRasterPos2sv);
  RasterPos3d := GetAddress('RasterPos3d',@glCapRasterPos3d);
  RasterPos3dv := GetAddress('RasterPos3dv',@glCapRasterPos3dv);
  RasterPos3f := GetAddress('RasterPos3f',@glCapRasterPos3f);
  RasterPos3fv := GetAddress('RasterPos3fv',@glCapRasterPos3fv);
  RasterPos3i := GetAddress('RasterPos3i',@glCapRasterPos3i);
  RasterPos3iv := GetAddress('RasterPos3iv',@glCapRasterPos3iv);
  RasterPos3s := GetAddress('RasterPos3s',@glCapRasterPos3s);
  RasterPos3sv := GetAddress('RasterPos3sv',@glCapRasterPos3sv);
  RasterPos4d := GetAddress('RasterPos4d',@glCapRasterPos4d);
  RasterPos4dv := GetAddress('RasterPos4dv',@glCapRasterPos4dv);
  RasterPos4f := GetAddress('RasterPos4f',@glCapRasterPos4f);
  RasterPos4fv := GetAddress('RasterPos4fv',@glCapRasterPos4fv);
  RasterPos4i := GetAddress('RasterPos4i',@glCapRasterPos4i);
  RasterPos4iv := GetAddress('RasterPos4iv',@glCapRasterPos4iv);
  RasterPos4s := GetAddress('RasterPos4s',@glCapRasterPos4s);
  RasterPos4sv := GetAddress('RasterPos4sv',@glCapRasterPos4sv);
  Rectd := GetAddress('Rectd',@glCapRectd);
  Rectdv := GetAddress('Rectdv',@glCapRectdv);
  Rectf := GetAddress('Rectf',@glCapRectf);
  Rectfv := GetAddress('Rectfv',@glCapRectfv);
  Recti := GetAddress('Recti',@glCapRecti);
  Rectiv := GetAddress('Rectiv',@glCapRectiv);
  Rects := GetAddress('Rects',@glCapRects);
  Rectsv := GetAddress('Rectsv',@glCapRectsv);
  RenderMode := GetAddress('RenderMode',@glCapRenderMode);
  Rotated := GetAddress('Rotated',@glCapRotated);
  Rotatef := GetAddress('Rotatef',@glCapRotatef);
  Scaled := GetAddress('Scaled',@glCapScaled);
  Scalef := GetAddress('Scalef',@glCapScalef);
  SelectBuffer := GetAddress('SelectBuffer',@glCapSelectBuffer);
  ShadeModel := GetAddress('ShadeModel',@glCapShadeModel);
  TexCoord1d := GetAddress('TexCoord1d',@glCapTexCoord1d);
  TexCoord1dv := GetAddress('TexCoord1dv',@glCapTexCoord1dv);
  TexCoord1f := GetAddress('TexCoord1f',@glCapTexCoord1f);
  TexCoord1fv := GetAddress('TexCoord1fv',@glCapTexCoord1fv);
  TexCoord1i := GetAddress('TexCoord1i',@glCapTexCoord1i);
  TexCoord1iv := GetAddress('TexCoord1iv',@glCapTexCoord1iv);
  TexCoord1s := GetAddress('TexCoord1s',@glCapTexCoord1s);
  TexCoord1sv := GetAddress('TexCoord1sv',@glCapTexCoord1sv);
  TexCoord2d := GetAddress('TexCoord2d',@glCapTexCoord2d);
  TexCoord2dv := GetAddress('TexCoord2dv',@glCapTexCoord2dv);
  TexCoord2f := GetAddress('TexCoord2f',@glCapTexCoord2f);
  TexCoord2fv := GetAddress('TexCoord2fv',@glCapTexCoord2fv);
  TexCoord2i := GetAddress('TexCoord2i',@glCapTexCoord2i);
  TexCoord2iv := GetAddress('TexCoord2iv',@glCapTexCoord2iv);
  TexCoord2s := GetAddress('TexCoord2s',@glCapTexCoord2s);
  TexCoord2sv := GetAddress('TexCoord2sv',@glCapTexCoord2sv);
  TexCoord3d := GetAddress('TexCoord3d',@glCapTexCoord3d);
  TexCoord3dv := GetAddress('TexCoord3dv',@glCapTexCoord3dv);
  TexCoord3f := GetAddress('TexCoord3f',@glCapTexCoord3f);
  TexCoord3fv := GetAddress('TexCoord3fv',@glCapTexCoord3fv);
  TexCoord3i := GetAddress('TexCoord3i',@glCapTexCoord3i);
  TexCoord3iv := GetAddress('TexCoord3iv',@glCapTexCoord3iv);
  TexCoord3s := GetAddress('TexCoord3s',@glCapTexCoord3s);
  TexCoord3sv := GetAddress('TexCoord3sv',@glCapTexCoord3sv);
  TexCoord4d := GetAddress('TexCoord4d',@glCapTexCoord4d);
  TexCoord4dv := GetAddress('TexCoord4dv',@glCapTexCoord4dv);
  TexCoord4f := GetAddress('TexCoord4f',@glCapTexCoord4f);
  TexCoord4fv := GetAddress('TexCoord4fv',@glCapTexCoord4fv);
  TexCoord4i := GetAddress('TexCoord4i',@glCapTexCoord4i);
  TexCoord4iv := GetAddress('TexCoord4iv',@glCapTexCoord4iv);
  TexCoord4s := GetAddress('TexCoord4s',@glCapTexCoord4s);
  TexCoord4sv := GetAddress('TexCoord4sv',@glCapTexCoord4sv);
  TexCoordPointer := GetAddress('TexCoordPointer',@glCapTexCoordPointer);
  TexEnvf := GetAddress('TexEnvf',@glCapTexEnvf);
  TexEnvfv := GetAddress('TexEnvfv',@glCapTexEnvfv);
  TexEnvi := GetAddress('TexEnvi',@glCapTexEnvi);
  TexEnviv := GetAddress('TexEnviv',@glCapTexEnviv);
  TexGend := GetAddress('TexGend',@glCapTexGend);
  TexGendv := GetAddress('TexGendv',@glCapTexGendv);
  TexGenf := GetAddress('TexGenf',@glCapTexGenf);
  TexGenfv := GetAddress('TexGenfv',@glCapTexGenfv);
  TexGeni := GetAddress('TexGeni',@glCapTexGeni);
  TexGeniv := GetAddress('TexGeniv',@glCapTexGeniv);
  Translated := GetAddress('Translated',@glCapTranslated);
  Translatef := GetAddress('Translatef',@glCapTranslatef);
  Vertex2d := GetAddress('Vertex2d',@glCapVertex2d);
  Vertex2dv := GetAddress('Vertex2dv',@glCapVertex2dv);
  Vertex2f := GetAddress('Vertex2f',@glCapVertex2f);
  Vertex2fv := GetAddress('Vertex2fv',@glCapVertex2fv);
  Vertex2i := GetAddress('Vertex2i',@glCapVertex2i);
  Vertex2iv := GetAddress('Vertex2iv',@glCapVertex2iv);
  Vertex2s := GetAddress('Vertex2s',@glCapVertex2s);
  Vertex2sv := GetAddress('Vertex2sv',@glCapVertex2sv);
  Vertex3d := GetAddress('Vertex3d',@glCapVertex3d);
  Vertex3dv := GetAddress('Vertex3dv',@glCapVertex3dv);
  Vertex3f := GetAddress('Vertex3f',@glCapVertex3f);
  Vertex3fv := GetAddress('Vertex3fv',@glCapVertex3fv);
  Vertex3i := GetAddress('Vertex3i',@glCapVertex3i);
  Vertex3iv := GetAddress('Vertex3iv',@glCapVertex3iv);
  Vertex3s := GetAddress('Vertex3s',@glCapVertex3s);
  Vertex3sv := GetAddress('Vertex3sv',@glCapVertex3sv);
  Vertex4d := GetAddress('Vertex4d',@glCapVertex4d);
  Vertex4dv := GetAddress('Vertex4dv',@glCapVertex4dv);
  Vertex4f := GetAddress('Vertex4f',@glCapVertex4f);
  Vertex4fv := GetAddress('Vertex4fv',@glCapVertex4fv);
  Vertex4i := GetAddress('Vertex4i',@glCapVertex4i);
  Vertex4iv := GetAddress('Vertex4iv',@glCapVertex4iv);
  Vertex4s := GetAddress('Vertex4s',@glCapVertex4s);
  Vertex4sv := GetAddress('Vertex4sv',@glCapVertex4sv);
  VertexPointer := GetAddress('VertexPointer',@glCapVertexPointer);
  BlendColor := GetAddress('BlendColor',@glCapBlendColor);
  BlendEquation := GetAddress('BlendEquation',@glCapBlendEquation);
  DrawRangeElements := GetAddress('DrawRangeElements',@glCapDrawRangeElements);
  TexImage3D := GetAddress('TexImage3D',@glCapTexImage3D);
  TexSubImage3D := GetAddress('TexSubImage3D',@glCapTexSubImage3D);
  CopyTexSubImage3D := GetAddress('CopyTexSubImage3D',@glCapCopyTexSubImage3D);

  IsRenderbuffer := GetAddress('IsRenderbuffer',@glCapIsRenderbuffer);
  BindRenderbuffer := GetAddress('BindRenderbuffer',@glCapBindRenderbuffer);
  DeleteRenderbuffers := GetAddress('DeleteRenderbuffers',@glCapDeleteRenderbuffers);
  GenRenderbuffers := GetAddress('GenRenderbuffers',@glCapGenRenderbuffers);
  RenderbufferStorage := GetAddress('RenderbufferStorage',@glCapRenderbufferStorage);
  RenderbufferStorageMultisample := GetAddress('RenderbufferStorageMultisample',@glCapRenderbufferStorageMultisample);
  GetRenderbufferParameteriv := GetAddress('GetRenderbufferParameteriv',@glCapGetRenderbufferParameteriv);
  IsFramebuffer := GetAddress('IsFramebuffer',@glCapIsFramebuffer);
  BindFramebuffer := GetAddress('BindFramebuffer',@glCapBindFramebuffer);
  DeleteFramebuffers := GetAddress('DeleteFramebuffers',@glCapDeleteFramebuffers);
  GenFramebuffers := GetAddress('GenFramebuffers',@glCapGenFramebuffers);
  CheckFramebufferStatus := GetAddress('CheckFramebufferStatus',@glCapCheckFramebufferStatus);
  FramebufferTexture := GetAddress('FramebufferTexture',@glCapFramebufferTexture);
  FramebufferTexture1D := GetAddress('FramebufferTexture1D',@glCapFramebufferTexture1D);
  FramebufferTexture2D := GetAddress('FramebufferTexture2D',@glCapFramebufferTexture2D);
  FramebufferTexture3D := GetAddress('FramebufferTexture3D',@glCapFramebufferTexture3D);
  FramebufferTextureLayer := GetAddress('FramebufferTextureLayer',@glCapFramebufferTextureLayer);
  FramebufferTextureFace := GetAddress('FramebufferTextureFace',@glCapFramebufferTextureFace);
  FramebufferRenderbuffer := GetAddress('FramebufferRenderbuffer',@glCapFramebufferRenderbuffer);
  GetFramebufferAttachmentParameteriv := GetAddress('GetFramebufferAttachmentParameteriv',@glCapGetFramebufferAttachmentParameteriv);
  BlitFramebuffer := GetAddress('BlitFramebuffer',@glCapBlitFramebuffer);
  GenerateMipmap := GetAddress('GenerateMipmap',@glCapGenerateMipmap);
  DepthBounds := GetAddress('DepthBounds',@glCapDepthBounds);
  ClearBufferiv := GetAddress('ClearBufferiv',@glCapClearBufferiv);
  ClearBufferuiv := GetAddress('ClearBufferuiv',@glCapClearBufferuiv);
  ClearBufferfv := GetAddress('ClearBufferfv',@glCapClearBufferfv);
  ClearBufferfi := GetAddress('ClearBufferfi',@glCapClearBufferfi);
  BlendFuncSeparate := GetAddress('BlendFuncSeparate',@glCapBlendFuncSeparate);
  MultiDrawArrays := GetAddress('MultiDrawArrays',@glCapMultiDrawArrays);
  MultiDrawElements := GetAddress('MultiDrawElements',@glCapMultiDrawElements);
  PointParameterf := GetAddress('PointParameterf',@glCapPointParameterf);
  PointParameterfv := GetAddress('PointParameterfv',@glCapPointParameterfv);
  PointParameteri := GetAddress('PointParameteri',@glCapPointParameteri);
  PointParameteriv := GetAddress('PointParameteriv',@glCapPointParameteriv);
  LockArrays := GetAddress('LockArrays',@glCapLockArrays);
  UnlockArrays := GetAddress('UnlockArrays',@glCapUnlockArrays);
  BindBuffer := GetAddress('BindBuffer',@glCapBindBuffer);
  DeleteBuffers := GetAddress('DeleteBuffers',@glCapDeleteBuffers);
  GenBuffers := GetAddress('GenBuffers',@glCapGenBuffers);
  IsBuffer := GetAddress('IsBuffer',@glCapIsBuffer);
  BufferData := GetAddress('BufferData',@glCapBufferData);
  BufferSubData := GetAddress('BufferSubData',@glCapBufferSubData);
  GetBufferSubData := GetAddress('GetBufferSubData',@glCapGetBufferSubData);
  MapBuffer := GetAddress('MapBuffer',@glCapMapBuffer);
  UnmapBuffer := GetAddress('UnmapBuffer',@glCapUnmapBuffer);
  GetBufferParameteriv := GetAddress('GetBufferParameteriv',@glCapGetBufferParameteriv);
  GetBufferPointerv := GetAddress('GetBufferPointerv',@glCapGetBufferPointerv);
  MapBufferRange := GetAddress('MapBufferRange',@glCapMapBufferRange);
  FlushMappedBufferRange := GetAddress('FlushMappedBufferRange',@glCapFlushMappedBufferRange);
  BindBufferRange := GetAddress('BindBufferRange',@glCapBindBufferRange);
  BindBufferOffset := GetAddress('BindBufferOffset',@glCapBindBufferOffset);
  BindBufferBase := GetAddress('BindBufferBase',@glCapBindBufferBase);
  BeginTransformFeedback := GetAddress('BeginTransformFeedback',@glCapBeginTransformFeedback);
  EndTransformFeedback := GetAddress('EndTransformFeedback',@glCapEndTransformFeedback);
  TransformFeedbackVaryings := GetAddress('TransformFeedbackVaryings',@glCapTransformFeedbackVaryings);
  GetTransformFeedbackVarying := GetAddress('GetTransformFeedbackVarying',@glCapGetTransformFeedbackVarying);

  TransformFeedbackAttribs := GetAddress('TransformFeedbackAttribs',@glCapTransformFeedbackAttribs);
  TransformFeedbackVaryingsNV := GetAddressNoSuffixes('TransformFeedbackVaryingsNV',@glCapTransformFeedbackVaryingsNV);
  TexBuffer := GetAddress('TexBuffer',@glCapTexBuffer);
  BindVertexArray := GetAddress('BindVertexArray',@glCapBindVertexArray);
  DeleteVertexArrays := GetAddress('DeleteVertexArrays',@glCapDeleteVertexArrays);
  GenVertexArrays := GetAddress('GenVertexArrays',@glCapGenVertexArrays);
  IsVertexArray := GetAddress('IsVertexArray',@glCapIsVertexArray);
  FlushVertexArrayRangeNV := GetAddressNoSuffixes('FlushVertexArrayRangeNV',@glCapFlushVertexArrayRangeNV);
  VertexArrayRangeNV := GetAddressNoSuffixes('VertexArrayRangeNV',@glCapVertexArrayRangeNV);
  CopyBufferSubData := GetAddress('CopyBufferSubData',@glCapCopyBufferSubData);
  UniformBuffer := GetAddress('UniformBuffer',@glCapUniformBuffer);
  GetUniformBufferSize := GetAddress('GetUniformBufferSize',@glCapGetUniformBufferSize);
  GetUniformOffset := GetAddress('GetUniformOffset',@glCapGetUniformOffset);
  PrimitiveRestartIndex := GetAddress('PrimitiveRestartIndex',@glCapPrimitiveRestartIndex);

  DrawElementsBaseVertex := GetAddress('DrawElementsBaseVertex',@glCapDrawElementsBaseVertex);
  DrawRangeElementsBaseVertex := GetAddress('DrawRangeElementsBaseVertex',@glCapDrawRangeElementsBaseVertex);
  DrawElementsInstancedBaseVertex := GetAddress('DrawElementsInstancedBaseVertex',@glCapDrawElementsInstancedBaseVertex);
  MultiDrawElementsBaseVertex := GetAddress('MultiDrawElementsBaseVertex',@glCapMultiDrawElementsBaseVertex);
  DrawArraysInstanced := GetAddress('DrawArraysInstanced',@glCapDrawArraysInstanced);
  DrawElementsInstanced := GetAddress('DrawElementsInstanced',@glCapDrawElementsInstanced);

  VertexAttrib1d := GetAddress('VertexAttrib1d',@glCapVertexAttrib1d);
  VertexAttrib1dv := GetAddress('VertexAttrib1dv',@glCapVertexAttrib1dv);
  VertexAttrib1f := GetAddress('VertexAttrib1f',@glCapVertexAttrib1f);
  VertexAttrib1fv := GetAddress('VertexAttrib1fv',@glCapVertexAttrib1fv);
  VertexAttrib1s := GetAddress('VertexAttrib1s',@glCapVertexAttrib1s);
  VertexAttrib1sv := GetAddress('VertexAttrib1sv',@glCapVertexAttrib1sv);
  VertexAttrib2d := GetAddress('VertexAttrib2d',@glCapVertexAttrib2d);
  VertexAttrib2dv := GetAddress('VertexAttrib2dv',@glCapVertexAttrib2dv);
  VertexAttrib2f := GetAddress('VertexAttrib2f',@glCapVertexAttrib2f);
  VertexAttrib2fv := GetAddress('VertexAttrib2fv',@glCapVertexAttrib2fv);
  VertexAttrib2s := GetAddress('VertexAttrib2s',@glCapVertexAttrib2s);
  VertexAttrib2sv := GetAddress('VertexAttrib2sv',@glCapVertexAttrib2sv);
  VertexAttrib3d := GetAddress('VertexAttrib3d',@glCapVertexAttrib3d);
  VertexAttrib3dv := GetAddress('VertexAttrib3dv',@glCapVertexAttrib3dv);
  VertexAttrib3f := GetAddress('VertexAttrib3f',@glCapVertexAttrib3f);
  VertexAttrib3fv := GetAddress('VertexAttrib3fv',@glCapVertexAttrib3fv);
  VertexAttrib3s := GetAddress('VertexAttrib3s',@glCapVertexAttrib3s);
  VertexAttrib3sv := GetAddress('VertexAttrib3sv',@glCapVertexAttrib3sv);
  VertexAttrib4Nbv := GetAddress('VertexAttrib4Nbv',@glCapVertexAttrib4Nbv);
  VertexAttrib4Niv := GetAddress('VertexAttrib4Niv',@glCapVertexAttrib4Niv);
  VertexAttrib4Nsv := GetAddress('VertexAttrib4Nsv',@glCapVertexAttrib4Nsv);
  VertexAttrib4Nub := GetAddress('VertexAttrib4Nub',@glCapVertexAttrib4Nub);
  VertexAttrib4Nubv := GetAddress('VertexAttrib4Nubv',@glCapVertexAttrib4Nubv);
  VertexAttrib4Nuiv := GetAddress('VertexAttrib4Nuiv',@glCapVertexAttrib4Nuiv);
  VertexAttrib4Nusv := GetAddress('VertexAttrib4Nusv',@glCapVertexAttrib4Nusv);
  VertexAttrib4bv := GetAddress('VertexAttrib4bv',@glCapVertexAttrib4bv);
  VertexAttrib4d := GetAddress('VertexAttrib4d',@glCapVertexAttrib4d);
  VertexAttrib4dv := GetAddress('VertexAttrib4dv',@glCapVertexAttrib4dv);
  VertexAttrib4f := GetAddress('VertexAttrib4f',@glCapVertexAttrib4f);
  VertexAttrib4fv := GetAddress('VertexAttrib4fv',@glCapVertexAttrib4fv);
  VertexAttrib4iv := GetAddress('VertexAttrib4iv',@glCapVertexAttrib4iv);
  VertexAttrib4s := GetAddress('VertexAttrib4s',@glCapVertexAttrib4s);
  VertexAttrib4sv := GetAddress('VertexAttrib4sv',@glCapVertexAttrib4sv);
  VertexAttrib4ubv := GetAddress('VertexAttrib4ubv',@glCapVertexAttrib4ubv);
  VertexAttrib4uiv := GetAddress('VertexAttrib4uiv',@glCapVertexAttrib4uiv);
  VertexAttrib4usv := GetAddress('VertexAttrib4usv',@glCapVertexAttrib4usv);
  VertexAttribPointer := GetAddress('VertexAttribPointer',@glCapVertexAttribPointer);
  VertexAttribI1i := GetAddress('VertexAttribI1i',@glCapVertexAttribI1i);
  VertexAttribI2i := GetAddress('VertexAttribI2i',@glCapVertexAttribI2i);
  VertexAttribI3i := GetAddress('VertexAttribI3i',@glCapVertexAttribI3i);
  VertexAttribI4i := GetAddress('VertexAttribI4i',@glCapVertexAttribI4i);
  VertexAttribI1ui := GetAddress('VertexAttribI1ui',@glCapVertexAttribI1ui);
  VertexAttribI2ui := GetAddress('VertexAttribI2ui',@glCapVertexAttribI2ui);
  VertexAttribI3ui := GetAddress('VertexAttribI3ui',@glCapVertexAttribI3ui);
  VertexAttribI4ui := GetAddress('VertexAttribI4ui',@glCapVertexAttribI4ui);
  VertexAttribI1iv := GetAddress('VertexAttribI1iv',@glCapVertexAttribI1iv);
  VertexAttribI2iv := GetAddress('VertexAttribI2iv',@glCapVertexAttribI2iv);
  VertexAttribI3iv := GetAddress('VertexAttribI3iv',@glCapVertexAttribI3iv);
  VertexAttribI4iv := GetAddress('VertexAttribI4iv',@glCapVertexAttribI4iv);
  VertexAttribI1uiv := GetAddress('VertexAttribI1uiv',@glCapVertexAttribI1uiv);
  VertexAttribI2uiv := GetAddress('VertexAttribI2uiv',@glCapVertexAttribI2uiv);
  VertexAttribI3uiv := GetAddress('VertexAttribI3uiv',@glCapVertexAttribI3uiv);
  VertexAttribI4uiv := GetAddress('VertexAttribI4uiv',@glCapVertexAttribI4uiv);
  VertexAttribI4bv := GetAddress('VertexAttribI4bv',@glCapVertexAttribI4bv);
  VertexAttribI4sv := GetAddress('VertexAttribI4sv',@glCapVertexAttribI4sv);
  VertexAttribI4ubv := GetAddress('VertexAttribI4ubv',@glCapVertexAttribI4ubv);
  VertexAttribI4usv := GetAddress('VertexAttribI4usv',@glCapVertexAttribI4usv);
  VertexAttribIPointer := GetAddress('VertexAttribIPointer',@glCapVertexAttribIPointer);
  GetVertexAttribIiv := GetAddress('GetVertexAttribIiv',@glCapGetVertexAttribIiv);
  GetVertexAttribIuiv := GetAddress('GetVertexAttribIuiv',@glCapGetVertexAttribIuiv);
  EnableVertexAttribArray := GetAddress('EnableVertexAttribArray',@glCapEnableVertexAttribArray);
  DisableVertexAttribArray := GetAddress('DisableVertexAttribArray',@glCapDisableVertexAttribArray);
  VertexAttribDivisor := GetAddress('VertexAttribDivisor',@glCapVertexAttribDivisor);

  GenQueries := GetAddress('GenQueries',@glCapGenQueries);
  DeleteQueries := GetAddress('DeleteQueries',@glCapDeleteQueries);
  IsQuery := GetAddress('IsQuery',@glCapIsQuery);
  BeginQuery := GetAddress('BeginQuery',@glCapBeginQuery);
  EndQuery := GetAddress('EndQuery',@glCapEndQuery);
  GetQueryiv := GetAddress('GetQueryiv',@glCapGetQueryiv);
  GetQueryObjectiv := GetAddress('GetQueryObjectiv',@glCapGetQueryObjectiv);
  GetQueryObjectuiv := GetAddress('GetQueryObjectuiv',@glCapGetQueryObjectuiv);
  QueryCounter := GetAddress('QueryCounter',@glCapQueryCounter);
  GetQueryObjecti64v := GetAddress('GetQueryObjecti64v',@glCapGetQueryObjecti64v);
  GetQueryObjectui64v := GetAddress('GetQueryObjectui64v',@glCapGetQueryObjectui64v);

  DeleteObject := GetAddress('DeleteObject',@glCapDeleteObject);
{$IFDEF GLS_OPENGL_ES}
  DeleteShader := GetAddress('DeleteShader',@glCapDeleteShader);
  DeleteProgram := GetAddress('DeleteProgram',@glCapDeleteProgram);
{$ENDIF}
  GetHandle := GetAddress('GetHandle',@glCapGetHandle);
  DetachShader := GetAddressAlt('DetachShader', 'DetachObject',@glCapDetachShader);
  CreateShader := GetAddressAlt('CreateShader', 'CreateShaderObject',@glCapCreateShader);
  ShaderSource := GetAddress('ShaderSource',@glCapShaderSource);
  CompileShader := GetAddress('CompileShader',@glCapCompileShader);
  CreateProgram := GetAddressAlt('CreateProgram', 'CreateProgramObject',@glCapCreateProgram);
  AttachShader := GetAddressAlt('AttachShader', 'AttachObject',@glCapAttachShader);
  LinkProgram := GetAddress('LinkProgram',@glCapLinkProgram);
  UseProgram := GetAddressAlt('UseProgram', 'UseProgramObject',@glCapUseProgram);
  ValidateProgram := GetAddress('ValidateProgram',@glCapValidateProgram);
  Uniform1f := GetAddress('Uniform1f',@glCapUniform1f);
  Uniform2f := GetAddress('Uniform2f',@glCapUniform2f);
  Uniform3f := GetAddress('Uniform3f',@glCapUniform3f);
  Uniform4f := GetAddress('Uniform4f',@glCapUniform4f);
  Uniform1i := GetAddress('Uniform1i',@glCapUniform1i);
  Uniform2i := GetAddress('Uniform2i',@glCapUniform2i);
  Uniform3i := GetAddress('Uniform3i',@glCapUniform3i);
  Uniform4i := GetAddress('Uniform4i',@glCapUniform4i);
  Uniform1fv := GetAddress('Uniform1fv',@glCapUniform1fv);
  Uniform2fv := GetAddress('Uniform2fv',@glCapUniform2fv);
  Uniform3fv := GetAddress('Uniform3fv',@glCapUniform3fv);
  Uniform4fv := GetAddress('Uniform4fv',@glCapUniform4fv);
  Uniform1iv := GetAddress('Uniform1iv',@glCapUniform1iv);
  Uniform2iv := GetAddress('Uniform2iv',@glCapUniform2iv);
  Uniform3iv := GetAddress('Uniform3iv',@glCapUniform3iv);
  Uniform4iv := GetAddress('Uniform4iv',@glCapUniform4iv);
  Uniform1ui := GetAddress('Uniform1ui',@glCapUniform1ui);
  Uniform2ui := GetAddress('Uniform2ui',@glCapUniform2ui);
  Uniform3ui := GetAddress('Uniform3ui',@glCapUniform3ui);
  Uniform4ui := GetAddress('Uniform4ui',@glCapUniform4ui);
  Uniform1uiv := GetAddress('Uniform1uiv',@glCapUniform1uiv);
  Uniform2uiv := GetAddress('Uniform2uiv',@glCapUniform2uiv);
  Uniform3uiv := GetAddress('Uniform3uiv',@glCapUniform3uiv);
  Uniform4uiv := GetAddress('Uniform4uiv',@glCapUniform4uiv);
  GetUniformuiv := GetAddress('GetUniformuiv',@glCapGetUniformuiv);
  UniformMatrix2fv := GetAddress('UniformMatrix2fv',@glCapUniformMatrix2fv);
  UniformMatrix3fv := GetAddress('UniformMatrix3fv',@glCapUniformMatrix3fv);
  UniformMatrix4fv := GetAddress('UniformMatrix4fv',@glCapUniformMatrix4fv);
  BindFragDataLocation := GetAddress('BindFragDataLocation',@glCapBindFragDataLocation);
  GetFragDataLocation := GetAddress('GetFragDataLocation',@glCapGetFragDataLocation);
  ClampColor := GetAddress('ClampColor',@glCapClampColor);
  ColorMaski := GetAddress('ColorMaski',@glCapColorMaski);
  GetBooleani_v := GetAddress('GetBooleani_v',@glCapGetBooleani_v);
  GetIntegeri_v := GetAddress('GetIntegeri_v',@glCapGetIntegeri_v);
  Enablei := GetAddress('Enablei',@glCapEnablei);
  Disablei := GetAddress('Disablei',@glCapDisablei);
  IsEnabledi := GetAddress('IsEnabledi',@glCapIsEnabledi);
  BindFragDataLocationIndexed := GetAddress('BindFragDataLocationIndexed',@glCapBindFragDataLocationIndexed);
  GetFragDataIndex := GetAddress('GetFragDataIndex',@glCapGetFragDataIndex);
  GetObjectParameterfv := GetAddress('GetObjectParameterfv',@glCapGetObjectParameterfv);
  GetObjectParameteriv := GetAddress('GetObjectParameteriv',@glCapGetObjectParameteriv);
  GetAttachedObjects := GetAddress('GetAttachedObjects',@glCapGetAttachedObjects);
  GetActiveAttrib := GetAddress('GetActiveAttrib',@glCapGetActiveAttrib);
  GetActiveUniform := GetAddress('GetActiveUniform',@glCapGetActiveUniform);
  GetAttachedShaders := GetAddress('GetAttachedShaders',@glCapGetAttachedShaders);
  GetAttribLocation := GetAddress('GetAttribLocation',@glCapGetAttribLocation);
  GetProgramiv := GetAddressAlt('GetProgramiv', 'GetObjectParameteriv',@glCapGetProgramiv);
  GetProgramInfoLog := GetAddress('GetProgramInfoLog',@glCapGetProgramInfoLog);
  GetShaderiv := GetAddressAlt('GetShaderiv', 'GetObjectParameteriv',@glCapGetShaderiv);
  GetInfoLog := GetAddress('GetInfoLog',@glCapGetInfoLog);
  GetShaderInfoLog := GetAddress('GetShaderInfoLog',@glCapGetShaderInfoLog);
  GetShaderSource := GetAddress('GetShaderSource',@glCapGetShaderSource);
  GetUniformLocation := GetAddress('GetUniformLocation',@glCapGetUniformLocation);
  GetUniformfv := GetAddress('GetUniformfv',@glCapGetUniformfv);
  GetUniformiv := GetAddress('GetUniformiv',@glCapGetUniformiv);
  GetVertexAttribdv := GetAddress('GetVertexAttribdv',@glCapGetVertexAttribdv);
  GetVertexAttribfv := GetAddress('GetVertexAttribfv',@glCapGetVertexAttribfv);
  GetVertexAttribiv := GetAddress('GetVertexAttribiv',@glCapGetVertexAttribiv);
  GetVertexAttribPointerv := GetAddress('GetVertexAttribPointerv',@glCapGetVertexAttribPointerv);
  IsProgram := GetAddress('IsProgram',@glCapIsProgram);
  IsShader := GetAddress('IsShader',@glCapIsShader);
  GetUniformLocation := GetAddress('GetUniformLocation',@glCapGetUniformLocation);
  BindAttribLocation := GetAddress('BindAttribLocation',@glCapBindAttribLocation);
  GetVaryingLocation := GetAddress('GetVaryingLocation',@glCapGetVaryingLocation);
  GetActiveVarying := GetAddress('GetActiveVarying',@glCapGetActiveVarying);
  ActiveVarying := GetAddress('ActiveVarying',@glCapActiveVarying);
  GetUniformIndices := GetAddress('GetUniformIndices',@glCapGetUniformIndices);
  GetActiveUniformsiv := GetAddress('GetActiveUniformsiv',@glCapGetActiveUniformsiv);
  GetActiveUniformName := GetAddress('GetActiveUniformName',@glCapGetActiveUniformName);
  GetUniformBlockIndex := GetAddress('GetUniformBlockIndex',@glCapGetUniformBlockIndex);
  GetActiveUniformBlockiv := GetAddress('GetActiveUniformBlockiv',@glCapGetActiveUniformBlockiv);
  GetActiveUniformBlockName := GetAddress('GetActiveUniformBlockName',@glCapGetActiveUniformBlockName);
  UniformBlockBinding := GetAddress('UniformBlockBinding',@glCapUniformBlockBinding);
  GetProgramBinary := GetAddress('GetProgramBinary',@glCapGetProgramBinary);
  ProgramBinary := GetAddress('ProgramBinary',@glCapProgramBinary);
  UseProgramStages := GetAddress('UseProgramStages',@glCapUseProgramStages);
  ActiveShaderProgram := GetAddress('ActiveShaderProgram',@glCapActiveShaderProgram);
  CreateShaderProgramv := GetAddress('CreateShaderProgramv',@glCapCreateShaderProgramv);
  BindProgramPipeline := GetAddress('BindProgramPipeline',@glCapBindProgramPipeline);
  DeleteProgramPipelines := GetAddress('DeleteProgramPipelines',@glCapDeleteProgramPipelines);
  GenProgramPipelines := GetAddress('GenProgramPipelines',@glCapGenProgramPipelines);
  IsProgramPipeline := GetAddress('IsProgramPipeline',@glCapIsProgramPipeline);
  GetProgramPipelineiv := GetAddress('GetProgramPipelineiv',@glCapGetProgramPipelineiv);
  ProgramUniform1i := GetAddress('ProgramUniform1i',@glCapProgramUniform1i);
  ProgramUniform1iv := GetAddress('ProgramUniform1iv',@glCapProgramUniform1iv);
  ProgramUniform1f := GetAddress('ProgramUniform1f',@glCapProgramUniform1f);
  ProgramUniform1fv := GetAddress('ProgramUniform1fv',@glCapProgramUniform1fv);
  ProgramUniform1d := GetAddress('ProgramUniform1d',@glCapProgramUniform1d);
  ProgramUniform1dv := GetAddress('ProgramUniform1dv',@glCapProgramUniform1dv);
  ProgramUniform1ui := GetAddress('ProgramUniform1ui',@glCapProgramUniform1ui);
  ProgramUniform1uiv := GetAddress('ProgramUniform1uiv',@glCapProgramUniform1uiv);
  ProgramUniform2i := GetAddress('ProgramUniform2i',@glCapProgramUniform2i);
  ProgramUniform2iv := GetAddress('ProgramUniform2iv',@glCapProgramUniform2iv);
  ProgramUniform2f := GetAddress('ProgramUniform2f',@glCapProgramUniform2f);
  ProgramUniform2fv := GetAddress('ProgramUniform2fv',@glCapProgramUniform2fv);
  ProgramUniform2d := GetAddress('ProgramUniform2d',@glCapProgramUniform2d);
  ProgramUniform2dv := GetAddress('ProgramUniform2dv',@glCapProgramUniform2dv);
  ProgramUniform2ui := GetAddress('ProgramUniform2ui',@glCapProgramUniform2ui);
  ProgramUniform2uiv := GetAddress('ProgramUniform2uiv',@glCapProgramUniform2uiv);
  ProgramUniform3i := GetAddress('ProgramUniform3i',@glCapProgramUniform3i);
  ProgramUniform3iv := GetAddress('ProgramUniform3iv',@glCapProgramUniform3iv);
  ProgramUniform3f := GetAddress('ProgramUniform3f',@glCapProgramUniform3f);
  ProgramUniform3fv := GetAddress('ProgramUniform3fv',@glCapProgramUniform3fv);
  ProgramUniform3d := GetAddress('ProgramUniform3d',@glCapProgramUniform3d);
  ProgramUniform3dv := GetAddress('ProgramUniform3dv',@glCapProgramUniform3dv);
  ProgramUniform3ui := GetAddress('ProgramUniform3ui',@glCapProgramUniform3ui);
  ProgramUniform3uiv := GetAddress('ProgramUniform3uiv',@glCapProgramUniform3uiv);
  ProgramUniform4i := GetAddress('ProgramUniform4i',@glCapProgramUniform4i);
  ProgramUniform4iv := GetAddress('ProgramUniform4iv',@glCapProgramUniform4iv);
  ProgramUniform4f := GetAddress('ProgramUniform4f',@glCapProgramUniform4f);
  ProgramUniform4fv := GetAddress('ProgramUniform4fv',@glCapProgramUniform4fv);
  ProgramUniform4d := GetAddress('ProgramUniform4d',@glCapProgramUniform4d);
  ProgramUniform4dv := GetAddress('ProgramUniform4dv',@glCapProgramUniform4dv);
  ProgramUniform4ui := GetAddress('ProgramUniform4ui',@glCapProgramUniform4ui);
  ProgramUniform4uiv := GetAddress('ProgramUniform4uiv',@glCapProgramUniform4uiv);
  ProgramUniformMatrix2fv := GetAddress('ProgramUniformMatrix2fv',@glCapProgramUniformMatrix2fv);
  ProgramUniformMatrix3fv := GetAddress('ProgramUniformMatrix3fv',@glCapProgramUniformMatrix3fv);
  ProgramUniformMatrix4fv := GetAddress('ProgramUniformMatrix4fv',@glCapProgramUniformMatrix4fv);
  ProgramUniformMatrix2dv := GetAddress('ProgramUniformMatrix2dv',@glCapProgramUniformMatrix2dv);
  ProgramUniformMatrix3dv := GetAddress('ProgramUniformMatrix3dv',@glCapProgramUniformMatrix3dv);
  ProgramUniformMatrix4dv := GetAddress('ProgramUniformMatrix4dv',@glCapProgramUniformMatrix4dv);
  ProgramUniformMatrix2x3fv := GetAddress('ProgramUniformMatrix2x3fv',@glCapProgramUniformMatrix2x3fv);
  ProgramUniformMatrix3x2fv := GetAddress('ProgramUniformMatrix3x2fv',@glCapProgramUniformMatrix3x2fv);
  ProgramUniformMatrix2x4fv := GetAddress('ProgramUniformMatrix2x4fv',@glCapProgramUniformMatrix2x4fv);
  ProgramUniformMatrix4x2fv := GetAddress('ProgramUniformMatrix4x2fv',@glCapProgramUniformMatrix4x2fv);
  ProgramUniformMatrix3x4fv := GetAddress('ProgramUniformMatrix3x4fv',@glCapProgramUniformMatrix3x4fv);
  ProgramUniformMatrix4x3fv := GetAddress('ProgramUniformMatrix4x3fv',@glCapProgramUniformMatrix4x3fv);
  ProgramUniformMatrix2x3dv := GetAddress('ProgramUniformMatrix2x3dv',@glCapProgramUniformMatrix2x3dv);
  ProgramUniformMatrix3x2dv := GetAddress('ProgramUniformMatrix3x2dv',@glCapProgramUniformMatrix3x2dv);
  ProgramUniformMatrix2x4dv := GetAddress('ProgramUniformMatrix2x4dv',@glCapProgramUniformMatrix2x4dv);
  ProgramUniformMatrix4x2dv := GetAddress('ProgramUniformMatrix4x2dv',@glCapProgramUniformMatrix4x2dv);
  ProgramUniformMatrix3x4dv := GetAddress('ProgramUniformMatrix3x4dv',@glCapProgramUniformMatrix3x4dv);
  ProgramUniformMatrix4x3dv := GetAddress('ProgramUniformMatrix4x3dv',@glCapProgramUniformMatrix4x3dv);
  ValidateProgramPipeline := GetAddress('ValidateProgramPipeline',@glCapValidateProgramPipeline);
  GetProgramPipelineInfoLog := GetAddress('GetProgramPipelineInfoLog',@glCapGetProgramPipelineInfoLog);

  BlendEquationSeparate := GetAddress('BlendEquationSeparate',@glCapBlendEquationSeparate);
  DrawBuffers := GetAddress('DrawBuffers',@glCapDrawBuffers);
  StencilOpSeparate := GetAddress('StencilOpSeparate',@glCapStencilOpSeparate);
  StencilFuncSeparate := GetAddress('StencilFuncSeparate',@glCapStencilFuncSeparate);
  StencilMaskSeparate := GetAddress('StencilMaskSeparate',@glCapStencilMaskSeparate);

  ActiveTexture := GetAddress('ActiveTexture',@glCapActiveTexture);
  CompressedTexImage3D := GetAddress('CompressedTexImage3D',@glCapCompressedTexImage3D);
  CompressedTexImage2D := GetAddress('CompressedTexImage2D',@glCapCompressedTexImage2D);
  CompressedTexImage1D := GetAddress('CompressedTexImage1D',@glCapCompressedTexImage1D);
  CompressedTexSubImage3D := GetAddress('CompressedTexSubImage3D',@glCapCompressedTexSubImage3D);
  CompressedTexSubImage2D := GetAddress('CompressedTexSubImage2D',@glCapCompressedTexSubImage2D);
  CompressedTexSubImage1D := GetAddress('CompressedTexSubImage1D',@glCapCompressedTexSubImage1D);
  GetCompressedTexImage := GetAddress('GetCompressedTexImage',@glCapGetCompressedTexImage);
  ClientActiveTexture := GetAddress('ClientActiveTexture',@glCapClientActiveTexture);
  MultiTexCoord1d := GetAddress('MultiTexCoord1d',@glCapMultiTexCoord1d);
  MultiTexCoord1dV := GetAddress('MultiTexCoord1dv',@glCapMultiTexCoord1dV);
  MultiTexCoord1f := GetAddress('MultiTexCoord1f',@glCapMultiTexCoord1f);
  MultiTexCoord1fv := GetAddress('MultiTexCoord1fv',@glCapMultiTexCoord1fv);
  MultiTexCoord1i := GetAddress('MultiTexCoord1i',@glCapMultiTexCoord1i);
  MultiTexCoord1iv := GetAddress('MultiTexCoord1iv',@glCapMultiTexCoord1iv);
  MultiTexCoord1s := GetAddress('MultiTexCoord1s',@glCapMultiTexCoord1s);
  MultiTexCoord1sv := GetAddress('MultiTexCoord1sv',@glCapMultiTexCoord1sv);
  MultiTexCoord2d := GetAddress('MultiTexCoord2d',@glCapMultiTexCoord2d);
  MultiTexCoord2dv := GetAddress('MultiTexCoord2dv',@glCapMultiTexCoord2dv);
  MultiTexCoord2f := GetAddress('MultiTexCoord2f',@glCapMultiTexCoord2f);
  MultiTexCoord2fv := GetAddress('MultiTexCoord2fv',@glCapMultiTexCoord2fv);
  MultiTexCoord2i := GetAddress('MultiTexCoord2i',@glCapMultiTexCoord2i);
  MultiTexCoord2iv := GetAddress('MultiTexCoord2iv',@glCapMultiTexCoord2iv);
  MultiTexCoord2s := GetAddress('MultiTexCoord2s',@glCapMultiTexCoord2s);
  MultiTexCoord2sv := GetAddress('MultiTexCoord2sv',@glCapMultiTexCoord2sv);
  MultiTexCoord3d := GetAddress('MultiTexCoord3d',@glCapMultiTexCoord3d);
  MultiTexCoord3dv := GetAddress('MultiTexCoord3dv',@glCapMultiTexCoord3dv);
  MultiTexCoord3f := GetAddress('MultiTexCoord3f',@glCapMultiTexCoord3f);
  MultiTexCoord3fv := GetAddress('MultiTexCoord3fv',@glCapMultiTexCoord3fv);
  MultiTexCoord3i := GetAddress('MultiTexCoord3i',@glCapMultiTexCoord3i);
  MultiTexCoord3iv := GetAddress('MultiTexCoord3iv',@glCapMultiTexCoord3iv);
  MultiTexCoord3s := GetAddress('MultiTexCoord3s',@glCapMultiTexCoord3s);
  MultiTexCoord3sv := GetAddress('MultiTexCoord3sv',@glCapMultiTexCoord3sv);
  MultiTexCoord4d := GetAddress('MultiTexCoord4d',@glCapMultiTexCoord4d);
  MultiTexCoord4dv := GetAddress('MultiTexCoord4dv',@glCapMultiTexCoord4dv);
  MultiTexCoord4f := GetAddress('MultiTexCoord4f',@glCapMultiTexCoord4f);
  MultiTexCoord4fv := GetAddress('MultiTexCoord4fv',@glCapMultiTexCoord4fv);
  MultiTexCoord4i := GetAddress('MultiTexCoord4i',@glCapMultiTexCoord4i);
  MultiTexCoord4iv := GetAddress('MultiTexCoord4iv',@glCapMultiTexCoord4iv);
  MultiTexCoord4s := GetAddress('MultiTexCoord4s',@glCapMultiTexCoord4s);
  MultiTexCoord4sv := GetAddress('MultiTexCoord4sv',@glCapMultiTexCoord4sv);

  GetInteger64i_v := GetAddress('GetInteger64i_v',@glCapGetInteger64i_v);
  GetBufferParameteri64v := GetAddress('GetBufferParameteri64v',@glCapGetBufferParameteri64v);
  ProgramParameteri := GetAddress('ProgramParameteri',@glCapProgramParameteri);

  ProgramString := GetAddress('ProgramString',@glCapProgramString);
  BindProgram := GetAddress('BindProgram',@glCapBindProgram);
  DeletePrograms := GetAddress('DeletePrograms',@glCapDeletePrograms);
  GenPrograms := GetAddress('GenPrograms',@glCapGenPrograms);
  ProgramEnvParameter4d := GetAddress('ProgramEnvParameter4d',@glCapProgramEnvParameter4d);
  ProgramEnvParameter4dv := GetAddress('ProgramEnvParameter4dv',@glCapProgramEnvParameter4dv);
  ProgramEnvParameter4f := GetAddress('ProgramEnvParameter4f',@glCapProgramEnvParameter4f);
  ProgramEnvParameter4fv := GetAddress('ProgramEnvParameter4fv',@glCapProgramEnvParameter4fv);
  ProgramLocalParameter4d := GetAddress('ProgramLocalParameter4d',@glCapProgramLocalParameter4d);
  ProgramLocalParameter4dv := GetAddress('ProgramLocalParameter4dv',@glCapProgramLocalParameter4dv);
  ProgramLocalParameter4f := GetAddress('ProgramLocalParameter4f',@glCapProgramLocalParameter4f);
  ProgramLocalParameter4fv := GetAddress('ProgramLocalParameter4fv',@glCapProgramLocalParameter4fv);
  GetProgramEnvParameterdv := GetAddress('GetProgramEnvParameterdv',@glCapGetProgramEnvParameterdv);
  GetProgramEnvParameterfv := GetAddress('GetProgramEnvParameterfv',@glCapGetProgramEnvParameterfv);
  GetProgramLocalParameterdv := GetAddress('GetProgramLocalParameterdv',@glCapGetProgramLocalParameterdv);
  GetProgramLocalParameterfv := GetAddress('GetProgramLocalParameterfv',@glCapGetProgramLocalParameterfv);

  ClearColorIi := GetAddress('ClearColorIi',@glCapClearColorIi);
  ClearColorIui := GetAddress('ClearColorIui',@glCapClearColorIui);
  TexParameterIiv := GetAddress('TexParameterIiv',@glCapTexParameterIiv);
  TexParameterIuiv := GetAddress('TexParameterIuiv',@glCapTexParameterIuiv);
  GetTexParameterIiv := GetAddress('GetTexParameterIiv',@glCapGetTexParameterIiv);
  GetTexParameterIuiv := GetAddress('GetTexParameterIuiv',@glCapGetTexParameterIuiv);
  PatchParameteri := GetAddress('PatchParameteri',@glCapPatchParameteri);
  PatchParameterfv := GetAddress('PatchParameterfv',@glCapPatchParameterfv);

  BufferAddressRangeNV := GetAddressNoSuffixes('BufferAddressRangeNV',@glCapBufferAddressRangeNV);
  VertexFormatNV := GetAddressNoSuffixes('VertexFormatNV',@glCapVertexFormatNV);
  NormalFormatNV := GetAddressNoSuffixes('NormalFormatNV',@glCapNormalFormatNV);
  ColorFormatNV := GetAddressNoSuffixes('ColorFormatNV',@glCapColorFormatNV);
  IndexFormatNV := GetAddressNoSuffixes('IndexFormatNV',@glCapIndexFormatNV);
  TexCoordFormatNV := GetAddressNoSuffixes('TexCoordFormatNV',@glCapTexCoordFormatNV);
  EdgeFlagFormatNV := GetAddressNoSuffixes('EdgeFlagFormatNV',@glCapEdgeFlagFormatNV);
  SecondaryColorFormatNV := GetAddressNoSuffixes('SecondaryColorFormatNV',@glCapSecondaryColorFormatNV);
  FogCoordFormatNV := GetAddressNoSuffixes('FogCoordFormatNV',@glCapFogCoordFormatNV);
  VertexAttribFormatNV := GetAddressNoSuffixes('VertexAttribFormatNV',@glCapVertexAttribFormatNV);
  VertexAttribIFormatNV := GetAddressNoSuffixes('VertexAttribIFormatNV',@glCapVertexAttribIFormatNV);
  GetIntegerui64i_vNV := GetAddressNoSuffixes('GetIntegerui64i_vNV',@glCapGetIntegerui64i_vNV);
  GetBufferParameterui64vNV := GetAddressNoSuffixes('GetBufferParameterui64vNV',@glCapGetBufferParameterui64vNV);
  MakeBufferResidentNV := GetAddressNoSuffixes('MakeBufferResidentNV',@glCapMakeBufferResidentNV);
  MakeBufferNonResidentNV := GetAddressNoSuffixes('MakeBufferNonResidentNV',@glCapMakeBufferNonResidentNV);
  IsBufferResidentNV := GetAddressNoSuffixes('IsBufferResidentNV',@glCapIsBufferResidentNV);
  MakeNamedBufferResidentNV := GetAddressNoSuffixes('MakeNamedBufferResidentNV',@glCapMakeNamedBufferResidentNV);
  MakeNamedBufferNonResidentNV := GetAddressNoSuffixes('MakeNamedBufferNonResidentNV',@glCapMakeNamedBufferNonResidentNV);
  IsNamedBufferResidentNV := GetAddressNoSuffixes('IsNamedBufferResidentNV',@glCapIsNamedBufferResidentNV);
  GetNamedBufferParameterui64vNV := GetAddressNoSuffixes('GetNamedBufferParameterui64vNV',@glCapGetNamedBufferParameterui64vNV);
  GetIntegerui64vNV := GetAddressNoSuffixes('GetIntegerui64vNV',@glCapGetIntegerui64vNV);
  Uniformui64NV := GetAddressNoSuffixes('Uniformui64NV',@glCapUniformui64NV);
  Uniformui64vNV := GetAddressNoSuffixes('Uniformui64vNV',@glCapUniformui64vNV);
  GetUniformui64vNV := GetAddressNoSuffixes('GetUniformui64vNV',@glCapGetUniformui64vNV);
  ProgramUniformui64NV := GetAddressNoSuffixes('ProgramUniformui64NV',@glCapProgramUniformui64NV);
  ProgramUniformui64vNV := GetAddressNoSuffixes('ProgramUniformui64vNV',@glCapProgramUniformui64vNV);

  TexImage2DMultisample := GetAddress('TexImage2DMultisample',@glCapTexImage2DMultisample);
  TexImage3DMultisample := GetAddress('TexImage3DMultisample',@glCapTexImage3DMultisample);
  GetMultisamplefv := GetAddress('GetMultisamplefv',@glCapGetMultisamplefv);
  SampleMaski := GetAddress('SampleMaski',@glCapSampleMaski);

  ProvokingVertex := GetAddress('ProvokingVertex',@glCapProvokingVertex);

  FenceSync := GetAddress('FenceSync',@glCapFenceSync);
  IsSync := GetAddress('IsSync',@glCapIsSync);
  DeleteSync := GetAddress('DeleteSync',@glCapDeleteSync);
  ClientWaitSync := GetAddress('ClientWaitSync',@glCapClientWaitSync);
  WaitSync := GetAddress('WaitSync',@glCapWaitSync);
  GetInteger64v := GetAddress('GetInteger64v',@glCapGetInteger64v);
  GetSynciv := GetAddress('GetSynciv',@glCapGetSynciv);

  BlendEquationi := GetAddress('BlendEquationi',@glCapBlendEquationi);
  BlendEquationSeparatei := GetAddress('BlendEquationSeparatei',@glCapBlendEquationSeparatei);
  BlendFunci := GetAddress('BlendFunci',@glCapBlendFunci);
  BlendFuncSeparatei := GetAddress('BlendFuncSeparatei',@glCapBlendFuncSeparatei);
  MinSampleShading := GetAddress('MinSampleShading',@glCapMinSampleShading);

  GenSamplers := GetAddress('GenSamplers',@glCapGenSamplers);
  DeleteSamplers := GetAddress('DeleteSamplers',@glCapDeleteSamplers);
  IsSampler := GetAddress('IsSampler',@glCapIsSampler);
  BindSampler := GetAddress('BindSampler',@glCapBindSampler);
  SamplerParameteri := GetAddress('SamplerParameteri',@glCapSamplerParameteri);
  SamplerParameteriv := GetAddress('SamplerParameteriv',@glCapSamplerParameteriv);
  SamplerParameterf := GetAddress('SamplerParameterf',@glCapSamplerParameterf);
  SamplerParameterfv := GetAddress('SamplerParameterfv',@glCapSamplerParameterfv);
  SamplerParameterIiv := GetAddress('SamplerParameterIiv',@glCapSamplerParameterIiv);
  SamplerParameterIuiv := GetAddress('SamplerParameterIuiv',@glCapSamplerParameterIuiv);
  GetSamplerParameteriv := GetAddress('GetSamplerParameteriv',@glCapGetSamplerParameteriv);
  GetSamplerParameterIiv := GetAddress('GetSamplerParameterIiv',@glCapGetSamplerParameterIiv);
  GetSamplerParameterfv := GetAddress('GetSamplerParameterfv',@glCapGetSamplerParameterfv);
  GetSamplerParameterIfv := GetAddress('GetSamplerParameterIfv',@glCapGetSamplerParameterIfv);

  ClientAttribDefault := GetAddress('ClientAttribDefault',@glCapClientAttribDefault);
  PushClientAttribDefault := GetAddress('PushClientAttribDefault',@glCapPushClientAttribDefault);
  MatrixLoadf := GetAddress('MatrixLoadf',@glCapMatrixLoadf);
  MatrixLoadd := GetAddress('MatrixLoadd',@glCapMatrixLoadd);
  MatrixMultf := GetAddress('MatrixMultf',@glCapMatrixMultf);
  MatrixMultd := GetAddress('MatrixMultd',@glCapMatrixMultd);
  MatrixLoadIdentity := GetAddress('MatrixLoadIdentity',@glCapMatrixLoadIdentity);
  MatrixRotatef := GetAddress('MatrixRotatef',@glCapMatrixRotatef);
  MatrixRotated := GetAddress('MatrixRotated',@glCapMatrixRotated);
  MatrixScalef := GetAddress('MatrixScalef',@glCapMatrixScalef);
  MatrixScaled := GetAddress('MatrixScaled',@glCapMatrixScaled);
  MatrixTranslatef := GetAddress('MatrixTranslatef',@glCapMatrixTranslatef);
  MatrixTranslated := GetAddress('MatrixTranslated',@glCapMatrixTranslated);
  MatrixFrustum := GetAddress('MatrixFrustum',@glCapMatrixFrustum);
  MatrixOrtho := GetAddress('MatrixOrtho',@glCapMatrixOrtho);
  MatrixPop := GetAddress('MatrixPop',@glCapMatrixPop);
  MatrixPush := GetAddress('MatrixPush',@glCapMatrixPush);
  MatrixLoadTransposef := GetAddress('MatrixLoadTransposef',@glCapMatrixLoadTransposef);
  MatrixLoadTransposed := GetAddress('MatrixLoadTransposed',@glCapMatrixLoadTransposed);
  MatrixMultTransposef := GetAddress('MatrixMultTransposef',@glCapMatrixMultTransposef);
  MatrixMultTransposed := GetAddress('MatrixMultTransposed',@glCapMatrixMultTransposed);
  TextureParameterf := GetAddress('TextureParameterf',@glCapTextureParameterf);
  TextureParameterfv := GetAddress('TextureParameterfv',@glCapTextureParameterfv);
  TextureParameteri := GetAddress('TextureParameteri',@glCapTextureParameteri);
  TextureParameteriv := GetAddress('TextureParameteriv',@glCapTextureParameteriv);
  TextureImage1D := GetAddress('TextureImage1D',@glCapTextureImage1D);
  TextureImage2D := GetAddress('TextureImage2D',@glCapTextureImage2D);
  TextureSubImage1D := GetAddress('TextureSubImage1D',@glCapTextureSubImage1D);
  TextureSubImage2D := GetAddress('TextureSubImage2D',@glCapTextureSubImage2D);
  CopyTextureImage1D := GetAddress('CopyTextureImage1D',@glCapCopyTextureImage1D);
  CopyTextureImage2D := GetAddress('CopyTextureImage2D',@glCapCopyTextureImage2D);
  CopyTextureSubImage1D := GetAddress('CopyTextureSubImage1D',@glCapCopyTextureSubImage1D);
  CopyTextureSubImage2D := GetAddress('CopyTextureSubImage2D',@glCapCopyTextureSubImage2D);
  GetTextureImage := GetAddress('GetTextureImage',@glCapGetTextureImage);
  GetTextureParameterfv := GetAddress('GetTextureParameterfv',@glCapGetTextureParameterfv);
  GetTextureParameteriv := GetAddress('GetTextureParameteriv',@glCapGetTextureParameteriv);
  GetTextureLevelParameterfv := GetAddress('GetTextureLevelParameterfv',@glCapGetTextureLevelParameterfv);
  GetTextureLevelParameteriv := GetAddress('GetTextureLevelParameteriv',@glCapGetTextureLevelParameteriv);
  TextureImage3D := GetAddress('TextureImage3D',@glCapTextureImage3D);
  TextureSubImage3D := GetAddress('TextureSubImage3D',@glCapTextureSubImage3D);
  CopyTextureSubImage3D := GetAddress('CopyTextureSubImage3D',@glCapCopyTextureSubImage3D);
  MultiTexParameterf := GetAddress('MultiTexParameterf',@glCapMultiTexParameterf);
  MultiTexParameterfv := GetAddress('MultiTexParameterfv',@glCapMultiTexParameterfv);
  MultiTexParameteri := GetAddress('MultiTexParameteri',@glCapMultiTexParameteri);
  MultiTexParameteriv := GetAddress('MultiTexParameteriv',@glCapMultiTexParameteriv);
  MultiTexImage1D := GetAddress('MultiTexImage1D',@glCapMultiTexImage1D);
  MultiTexImage2D := GetAddress('MultiTexImage2D',@glCapMultiTexImage2D);
  MultiTexSubImage1D := GetAddress('MultiTexSubImage1D',@glCapMultiTexSubImage1D);
  MultiTexSubImage2D := GetAddress('MultiTexSubImage2D',@glCapMultiTexSubImage2D);
  CopyMultiTexImage1D := GetAddress('CopyMultiTexImage1D',@glCapCopyMultiTexImage1D);
  CopyMultiTexImage2D := GetAddress('CopyMultiTexImage2D',@glCapCopyMultiTexImage2D);
  CopyMultiTexSubImage1D := GetAddress('CopyMultiTexSubImage1D',@glCapCopyMultiTexSubImage1D);
  CopyMultiTexSubImage2D := GetAddress('CopyMultiTexSubImage2D',@glCapCopyMultiTexSubImage2D);
  GetMultiTexImage := GetAddress('GetMultiTexImage',@glCapGetMultiTexImage);
  GetMultiTexParameterfv := GetAddress('GetMultiTexParameterfv',@glCapGetMultiTexParameterfv);
  GetMultiTexParameteriv := GetAddress('GetMultiTexParameteriv',@glCapGetMultiTexParameteriv);
  GetMultiTexLevelParameterfv := GetAddress('GetMultiTexLevelParameterfv',@glCapGetMultiTexLevelParameterfv);
  GetMultiTexLevelParameteriv := GetAddress('GetMultiTexLevelParameteriv',@glCapGetMultiTexLevelParameteriv);
  MultiTexImage3D := GetAddress('MultiTexImage3D',@glCapMultiTexImage3D);
  MultiTexSubImage3D := GetAddress('MultiTexSubImage3D',@glCapMultiTexSubImage3D);
  CopyMultiTexSubImage3D := GetAddress('CopyMultiTexSubImage3D',@glCapCopyMultiTexSubImage3D);
  BindMultiTexture := GetAddress('BindMultiTexture',@glCapBindMultiTexture);
  EnableClientStateIndexed := GetAddress('EnableClientStateIndexed',@glCapEnableClientStateIndexed);
  DisableClientStateIndexed := GetAddress('DisableClientStateIndexed',@glCapDisableClientStateIndexed);
  MultiTexCoordPointer := GetAddress('MultiTexCoordPointer',@glCapMultiTexCoordPointer);
  MultiTexEnvf := GetAddress('MultiTexEnvf',@glCapMultiTexEnvf);
  MultiTexEnvfv := GetAddress('MultiTexEnvfv',@glCapMultiTexEnvfv);
  MultiTexEnvi := GetAddress('MultiTexEnvi',@glCapMultiTexEnvi);
  MultiTexEnviv := GetAddress('MultiTexEnviv',@glCapMultiTexEnviv);
  MultiTexGend := GetAddress('MultiTexGend',@glCapMultiTexGend);
  MultiTexGendv := GetAddress('MultiTexGendv',@glCapMultiTexGendv);
  MultiTexGenf := GetAddress('MultiTexGenf',@glCapMultiTexGenf);
  MultiTexGenfv := GetAddress('MultiTexGenfv',@glCapMultiTexGenfv);
  MultiTexGeni := GetAddress('MultiTexGeni',@glCapMultiTexGeni);
  MultiTexGeniv := GetAddress('MultiTexGeniv',@glCapMultiTexGeniv);
  GetMultiTexEnvfv := GetAddress('GetMultiTexEnvfv',@glCapGetMultiTexEnvfv);
  GetMultiTexEnviv := GetAddress('GetMultiTexEnviv',@glCapGetMultiTexEnviv);
  GetMultiTexGendv := GetAddress('GetMultiTexGendv',@glCapGetMultiTexGendv);
  GetMultiTexGenfv := GetAddress('GetMultiTexGenfv',@glCapGetMultiTexGenfv);
  GetMultiTexGeniv := GetAddress('GetMultiTexGeniv',@glCapGetMultiTexGeniv);
  GetFloatIndexedv := GetAddress('GetFloatIndexedv',@glCapGetFloatIndexedv);
  GetDoubleIndexedv := GetAddress('GetDoubleIndexedv',@glCapGetDoubleIndexedv);
  GetPointerIndexedv := GetAddress('GetPointerIndexedv',@glCapGetPointerIndexedv);
  CompressedTextureImage3D := GetAddress('CompressedTextureImage3D',@glCapCompressedTextureImage3D);
  CompressedTextureImage2D := GetAddress('CompressedTextureImage2D',@glCapCompressedTextureImage2D);
  CompressedTextureImage1D := GetAddress('CompressedTextureImage1D',@glCapCompressedTextureImage1D);
  CompressedTextureSubImage3D := GetAddress('CompressedTextureSubImage3D',@glCapCompressedTextureSubImage3D);
  CompressedTextureSubImage2D := GetAddress('CompressedTextureSubImage2D',@glCapCompressedTextureSubImage2D);
  CompressedTextureSubImage1D := GetAddress('CompressedTextureSubImage1D',@glCapCompressedTextureSubImage1D);
  GetCompressedTextureImage := GetAddress('GetCompressedTextureImage',@glCapGetCompressedTextureImage);
  CompressedMultiTexImage3D := GetAddress('CompressedMultiTexImage3D',@glCapCompressedMultiTexImage3D);
  CompressedMultiTexImage2D := GetAddress('CompressedMultiTexImage2D',@glCapCompressedMultiTexImage2D);
  CompressedMultiTexImage1D := GetAddress('CompressedMultiTexImage1D',@glCapCompressedMultiTexImage1D);
  CompressedMultiTexSubImage3D := GetAddress('CompressedMultiTexSubImage3D',@glCapCompressedMultiTexSubImage3D);
  CompressedMultiTexSubImage2D := GetAddress('CompressedMultiTexSubImage2D',@glCapCompressedMultiTexSubImage2D);
  CompressedMultiTexSubImage1D := GetAddress('CompressedMultiTexSubImage1D',@glCapCompressedMultiTexSubImage1D);
  GetCompressedMultiTexImage := GetAddress('GetCompressedMultiTexImage',@glCapGetCompressedMultiTexImage);
  NamedProgramString := GetAddress('NamedProgramString',@glCapNamedProgramString);
  NamedProgramLocalParameter4d := GetAddress('NamedProgramLocalParameter4d',@glCapNamedProgramLocalParameter4d);
  NamedProgramLocalParameter4dv := GetAddress('NamedProgramLocalParameter4dv',@glCapNamedProgramLocalParameter4dv);
  NamedProgramLocalParameter4f := GetAddress('NamedProgramLocalParameter4f',@glCapNamedProgramLocalParameter4f);
  NamedProgramLocalParameter4fv := GetAddress('NamedProgramLocalParameter4fv',@glCapNamedProgramLocalParameter4fv);
  GetNamedProgramLocalParameterdv := GetAddress('GetNamedProgramLocalParameterdv',@glCapGetNamedProgramLocalParameterdv);
  GetNamedProgramLocalParameterfv := GetAddress('GetNamedProgramLocalParameterfv',@glCapGetNamedProgramLocalParameterfv);
  GetNamedProgramiv := GetAddress('GetNamedProgramiv',@glCapGetNamedProgramiv);
  GetNamedProgramString := GetAddress('GetNamedProgramString',@glCapGetNamedProgramString);
  NamedProgramLocalParameters4fv := GetAddress('NamedProgramLocalParameters4fv',@glCapNamedProgramLocalParameters4fv);
  NamedProgramLocalParameterI4i := GetAddress('NamedProgramLocalParameterI4i',@glCapNamedProgramLocalParameterI4i);
  NamedProgramLocalParameterI4iv := GetAddress('NamedProgramLocalParameterI4iv',@glCapNamedProgramLocalParameterI4iv);
  NamedProgramLocalParametersI4iv := GetAddress('NamedProgramLocalParametersI4iv',@glCapNamedProgramLocalParametersI4iv);
  NamedProgramLocalParameterI4ui := GetAddress('NamedProgramLocalParameterI4ui',@glCapNamedProgramLocalParameterI4ui);
  NamedProgramLocalParameterI4uiv := GetAddress('NamedProgramLocalParameterI4uiv',@glCapNamedProgramLocalParameterI4uiv);
  NamedProgramLocalParametersI4uiv := GetAddress('NamedProgramLocalParametersI4uiv',@glCapNamedProgramLocalParametersI4uiv);
  GetNamedProgramLocalParameterIiv := GetAddress('GetNamedProgramLocalParameterIiv',@glCapGetNamedProgramLocalParameterIiv);
  GetNamedProgramLocalParameterIuiv := GetAddress('GetNamedProgramLocalParameterIuiv',@glCapGetNamedProgramLocalParameterIuiv);
  TextureParameterIiv := GetAddress('TextureParameterIiv',@glCapTextureParameterIiv);
  TextureParameterIuiv := GetAddress('TextureParameterIuiv',@glCapTextureParameterIuiv);
  GetTextureParameterIiv := GetAddress('GetTextureParameterIiv',@glCapGetTextureParameterIiv);
  GetTextureParameterIuiv := GetAddress('GetTextureParameterIuiv',@glCapGetTextureParameterIuiv);
  MultiTexParameterIiv := GetAddress('MultiTexParameterIiv',@glCapMultiTexParameterIiv);
  MultiTexParameterIuiv := GetAddress('MultiTexParameterIuiv',@glCapMultiTexParameterIuiv);
  GetMultiTexParameterIiv := GetAddress('GetMultiTexParameterIiv',@glCapGetMultiTexParameterIiv);
  GetMultiTexParameterIuiv := GetAddress('GetMultiTexParameterIuiv',@glCapGetMultiTexParameterIuiv);
  NamedBufferData := GetAddress('NamedBufferData',@glCapNamedBufferData);
  NamedBufferSubData := GetAddress('NamedBufferSubData',@glCapNamedBufferSubData);
  MapNamedBuffer := GetAddress('MapNamedBuffer',@glCapMapNamedBuffer);
  UnmapNamedBuffer := GetAddress('UnmapNamedBuffer',@glCapUnmapNamedBuffer);
  MapNamedBufferRange := GetAddress('MapNamedBufferRange',@glCapMapNamedBufferRange);
  FlushMappedNamedBufferRange := GetAddress('FlushMappedNamedBufferRange',@glCapFlushMappedNamedBufferRange);
  NamedCopyBufferSubData := GetAddress('NamedCopyBufferSubData',@glCapNamedCopyBufferSubData);
  GetNamedBufferParameteriv := GetAddress('GetNamedBufferParameteriv',@glCapGetNamedBufferParameteriv);
  GetNamedBufferPointerv := GetAddress('GetNamedBufferPointerv',@glCapGetNamedBufferPointerv);
  GetNamedBufferSubData := GetAddress('GetNamedBufferSubData',@glCapGetNamedBufferSubData);
  TextureBuffer := GetAddress('TextureBuffer',@glCapTextureBuffer);
  MultiTexBuffer := GetAddress('MultiTexBuffer',@glCapMultiTexBuffer);
  NamedRenderbufferStorage := GetAddress('NamedRenderbufferStorage',@glCapNamedRenderbufferStorage);
  GetNamedRenderbufferParameteriv := GetAddress('GetNamedRenderbufferParameteriv',@glCapGetNamedRenderbufferParameteriv);
  CheckNamedFramebufferStatus := GetAddress('CheckNamedFramebufferStatus',@glCapCheckNamedFramebufferStatus);
  NamedFramebufferTexture1D := GetAddress('NamedFramebufferTexture1D',@glCapNamedFramebufferTexture1D);
  NamedFramebufferTexture2D := GetAddress('NamedFramebufferTexture2D',@glCapNamedFramebufferTexture2D);
  NamedFramebufferTexture3D := GetAddress('NamedFramebufferTexture3D',@glCapNamedFramebufferTexture3D);
  NamedFramebufferRenderbuffer := GetAddress('NamedFramebufferRenderbuffer',@glCapNamedFramebufferRenderbuffer);
  GetNamedFramebufferAttachmentParameteriv := GetAddress('GetNamedFramebufferAttachmentParameteriv',@glCapGetNamedFramebufferAttachmentParameteriv);
  GenerateTextureMipmap := GetAddress('GenerateTextureMipmap',@glCapGenerateTextureMipmap);
  GenerateMultiTexMipmap := GetAddress('GenerateMultiTexMipmap',@glCapGenerateMultiTexMipmap);
  FramebufferDrawBuffer := GetAddress('FramebufferDrawBuffer',@glCapFramebufferDrawBuffer);
  FramebufferDrawBuffers := GetAddress('FramebufferDrawBuffers',@glCapFramebufferDrawBuffers);
  FramebufferReadBuffer := GetAddress('FramebufferReadBuffer',@glCapFramebufferReadBuffer);
  GetFramebufferParameteriv := GetAddress('GetFramebufferParameteriv',@glCapGetFramebufferParameteriv);
  NamedRenderbufferStorageMultisample := GetAddress('NamedRenderbufferStorageMultisample',@glCapNamedRenderbufferStorageMultisample);
  NamedRenderbufferStorageMultisampleCoverage := GetAddress('NamedRenderbufferStorageMultisampleCoverage',@glCapNamedRenderbufferStorageMultisampleCoverage);
  NamedFramebufferTexture := GetAddress('NamedFramebufferTexture',@glCapNamedFramebufferTexture);
  NamedFramebufferTextureLayer := GetAddress('NamedFramebufferTextureLayer',@glCapNamedFramebufferTextureLayer);
  NamedFramebufferTextureFace := GetAddress('NamedFramebufferTextureFace',@glCapNamedFramebufferTextureFace);
  TextureRenderbuffer := GetAddress('TextureRenderbuffer',@glCapTextureRenderbuffer);
  MultiTexRenderbuffer := GetAddress('MultiTexRenderbuffer',@glCapMultiTexRenderbuffer);


  FrameTerminatorGREMEDY := GetAddress('FrameTerminatorGREMEDY',@glCapFrameTerminatorGREMEDY);
  StringMarkerGREMEDY := GetAddress('StringMarkerGREMEDY',@glCapStringMarkerGREMEDY);
  DebugMessageEnableAMDX := GetAddressNoSuffixes('DebugMessageEnableAMDX',@glCapDebugMessageEnableAMDX);
  DebugMessageCallbackAMDX := GetAddressNoSuffixes('DebugMessageCallbackAMDX',@glCapDebugMessageCallbackAMDX);
  DebugMessageControl := GetAddress('DebugMessageControl',@glCapDebugMessageControl);
  DebugMessageInsert := GetAddress('DebugMessageInsert',@glCapDebugMessageInsert);
  DebugMessageCallback := GetAddress('DebugMessageCallback',@glCapDebugMessageCallback);
  GetDebugMessageLog := GetAddress('GetDebugMessageLog',@glCapGetDebugMessageLog);

{$IFDEF LINUX}
  VDPAUInitNV := GetAddressNoSuffixes('VDPAUInitNV',@glCapVDPAUInitNV);
  VDPAUFiniNV := GetAddressNoSuffixes('VDPAUFiniNV',@glCapVDPAUFiniNV);
  VDPAURegisterVideoSurfaceNV := GetAddressNoSuffixes('VDPAURegisterVideoSurfaceNV',@glCapVDPAURegisterVideoSurfaceNV);
  VDPAURegisterOutputSurfaceNV := GetAddressNoSuffixes('VDPAURegisterOutputSurfaceNV',@glCapVDPAURegisterOutputSurfaceNV);
  VDPAUIsSurfaceNV := GetAddressNoSuffixes('VDPAUIsSurfaceNV',@glCapVDPAUIsSurfaceNV);
  VDPAUUnregisterSurfaceNV := GetAddressNoSuffixes('VDPAUUnregisterSurfaceNV',@glCapVDPAUUnregisterSurfaceNV);
  VDPAUGetSurfaceivNV := GetAddressNoSuffixes('VDPAUGetSurfaceivNV',@glCapVDPAUGetSurfaceivNV);
  VDPAUSurfaceAccessNV := GetAddressNoSuffixes('VDPAUSurfaceAccessNV',@glCapVDPAUSurfaceAccessNV);
  VDPAUMapSurfacesNV := GetAddressNoSuffixes('VDPAUMapSurfacesNV',@glCapVDPAUMapSurfacesNV);
  VDPAUUnmapSurfacesNV := GetAddressNoSuffixes('VDPAUUnmapSurfacesNV',@glCapVDPAUUnmapSurfacesNV);
{$ENDIF LINUX}

  GenPathsNV := GetAddressNoSuffixes('GenPathsNV',@glCapGenPathsNV);
  DeletePathsNV := GetAddressNoSuffixes('DeletePathsNV',@glCapDeletePathsNV);
  IsPathNV := GetAddressNoSuffixes('IsPathNV',@glCapIsPathNV);
  PathCommandsNV := GetAddressNoSuffixes('PathCommandsNV',@glCapPathCommandsNV);
  PathCoordsNV := GetAddressNoSuffixes('PathCoordsNV',@glCapPathCoordsNV);
  PathSubCommandsNV := GetAddressNoSuffixes('PathSubCommandsNV',@glCapPathSubCommandsNV);
  PathSubCoordsNV := GetAddressNoSuffixes('PathSubCoordsNV',@glCapPathSubCoordsNV);
  PathStringNV := GetAddressNoSuffixes('PathStringNV',@glCapPathStringNV);
  PathGlyphsNV := GetAddressNoSuffixes('PathGlyphsNV',@glCapPathGlyphsNV);
  PathGlyphRangeNV := GetAddressNoSuffixes('PathGlyphRangeNV',@glCapPathGlyphRangeNV);
  WeightPathsNV := GetAddressNoSuffixes('WeightPathsNV',@glCapWeightPathsNV);
  CopyPathNV := GetAddressNoSuffixes('CopyPathNV',@glCapCopyPathNV);
  InterpolatePathsNV := GetAddressNoSuffixes('InterpolatePathsNV',@glCapInterpolatePathsNV);
  PathParameterivNV := GetAddressNoSuffixes('PathParameterivNV',@glCapPathParameterivNV);
  PathParameteriNV := GetAddressNoSuffixes('PathParameteriNV',@glCapPathParameteriNV);
  PathParameterfvNV := GetAddressNoSuffixes('PathParameterfvNV',@glCapPathParameterfvNV);
  PathParameterfNV := GetAddressNoSuffixes('PathParameterfNV',@glCapPathParameterfNV);
  PathDashArrayNV := GetAddressNoSuffixes('PathDashArrayNV',@glCapPathDashArrayNV);
  PathStencilFuncNV := GetAddressNoSuffixes('PathStencilFuncNV',@glCapPathStencilFuncNV);
  StencilFillPathNV := GetAddressNoSuffixes('StencilFillPathNV',@glCapStencilFillPathNV);
  StencilStrokePathNV := GetAddressNoSuffixes('StencilStrokePathNV',@glCapStencilStrokePathNV);
  StencilFillPathInstancedNV := GetAddressNoSuffixes('StencilFillPathInstancedNV',@glCapStencilFillPathInstancedNV);
  StencilStrokePathInstancedNV := GetAddressNoSuffixes('StencilStrokePathInstancedNV',@glCapStencilStrokePathInstancedNV);
  PathColorGenNV := GetAddressNoSuffixes('PathColorGenNV',@glCapPathColorGenNV);
  PathTexGenNV := GetAddressNoSuffixes('PathTexGenNV',@glCapPathTexGenNV);
  PathFogGenNV := GetAddressNoSuffixes('PathFogGenNV',@glCapPathFogGenNV);
  CoverFillPathNV := GetAddressNoSuffixes('CoverFillPathNV',@glCapCoverFillPathNV);
  CoverStrokePathNV := GetAddressNoSuffixes('CoverStrokePathNV',@glCapCoverStrokePathNV);
  CoverFillPathInstancedNV := GetAddressNoSuffixes('CoverFillPathInstancedNV',@glCapCoverFillPathInstancedNV);
  CoverStrokePathInstancedNV := GetAddressNoSuffixes('CoverStrokePathInstancedNV',@glCapCoverStrokePathInstancedNV);
  GetPathParameterivNV := GetAddressNoSuffixes('GetPathParameterivNV',@glCapGetPathParameterivNV);
  GetPathParameterfvNV := GetAddressNoSuffixes('GetPathParameterfvNV',@glCapGetPathParameterfvNV);
  GetPathCommandsNV := GetAddressNoSuffixes('GetPathCommandsNV',@glCapGetPathCommandsNV);
  GetPathCoordsNV := GetAddressNoSuffixes('GetPathCoordsNV',@glCapGetPathCoordsNV);
  GetPathDashArrayNV := GetAddressNoSuffixes('GetPathDashArrayNV',@glCapGetPathDashArrayNV);
  GetPathMetricsNV := GetAddressNoSuffixes('GetPathMetricsNV',@glCapGetPathMetricsNV);
  GetPathMetricRangeNV := GetAddressNoSuffixes('GetPathMetricRangeNV',@glCapGetPathMetricRangeNV);
  GetPathSpacingNV := GetAddressNoSuffixes('GetPathSpacingNV',@glCapGetPathSpacingNV);
  GetPathColorGenivNV := GetAddressNoSuffixes('GetPathColorGenivNV',@glCapGetPathColorGenivNV);
  GetPathColorGenfvNV := GetAddressNoSuffixes('GetPathColorGenfvNV',@glCapGetPathColorGenfvNV);
  GetPathTexGenivNV := GetAddressNoSuffixes('GetPathTexGenivNV',@glCapGetPathTexGenivNV);
  GetPathTexGenfvNV := GetAddressNoSuffixes('GetPathTexGenfvNV',@glCapGetPathTexGenfvNV);
  IsPointInFillPathNV := GetAddressNoSuffixes('IsPointInFillPathNV',@glCapIsPointInFillPathNV);
  IsPointInStrokePathNV := GetAddressNoSuffixes('IsPointInStrokePathNV',@glCapIsPointInStrokePathNV);
  GetPathLengthNV := GetAddressNoSuffixes('GetPathLengthNV',@glCapGetPathLengthNV);
  PointAlongPathNV := GetAddressNoSuffixes('PointAlongPathNV',@glCapPointAlongPathNV);
  PathStencilDepthOffsetNV := GetAddressNoSuffixes('PathStencilDepthOffsetNV',@glCapPathStencilDepthOffsetNV);
  PathCoverDepthFuncNV := GetAddressNoSuffixes('PathCoverDepthFuncNV',@glCapPathCoverDepthFuncNV);

  if FDebug then
    if ARB_debug_output then
    begin
      DebugMessageCallback(DebugCallBack, nil);
      DebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, FDebugIds, True);
      Enable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
    end
    else if AMDX_debug_output then
    begin
      DebugMessageCallbackAMDX(DebugCallBackAMD, nil);
      DebugMessageEnableAMDX(0, 0, 0, FDebugIds, True);
    end
    else
      FDebug := False;

  SetLength(FBuffer, 0);
  FInitialized := True;
end;

procedure TGLExtensionsAndEntryPoints.Close;
begin
  if FDebug then

    if ARB_debug_output then
    begin
      DebugMessageCallback(nil, nil);
      DebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, FDebugIds, False);
    end
    else if AMDX_debug_output then
    begin
      DebugMessageCallbackAMDX(nil, nil);
      DebugMessageEnableAMDX(0, 0, 0, FDebugIds, False);
    end;

  VERSION_1_0 := False;
  VERSION_1_1 := False;
  VERSION_1_2 := False;
  VERSION_1_3 := False;
  VERSION_1_4 := False;
  VERSION_1_5 := False;
  VERSION_2_0 := False;
  VERSION_2_1 := False;
  VERSION_3_0 := False;
  VERSION_3_1 := False;
  VERSION_3_2 := False;
  VERSION_3_3 := False;
  VERSION_4_0 := False;
  VERSION_4_1 := False;

  ARB_blend_func_extended := False;
  ARB_color_buffer_float := False;
  ARB_compatibility := False;
  ARB_copy_buffer := False;
  ARB_depth_buffer_float := False;
  ARB_depth_clamp := False;
  ARB_depth_texture := False;
  ARB_draw_buffers := False;
  ARB_draw_buffers_blend := False;
  ARB_draw_elements_base_vertex := False;
  ARB_draw_indirect := False;
  ARB_draw_instanced := False;
  ARB_explicit_attrib_location := False;
  ARB_fragment_coord_conventions := False;
  ARB_fragment_program := False;
  ARB_fragment_program_shadow := False;
  ARB_fragment_shader := False;
  ARB_framebuffer_object := False;
  ARB_framebuffer_sRGB := False;
  ARB_geometry_shader4 := False;
  ARB_gpu_shader_fp64 := False;
  ARB_gpu_shader5 := False;
  ARB_half_float_pixel := False;
  ARB_half_float_vertex := False;
  ARB_imaging := False;
  ARB_instanced_arrays := False;
  ARB_map_buffer_range := False;
  ARB_matrix_palette := False;
  ARB_multisample := False;
  // ' ' to avoid collision with WGL variant
  ARB_multitexture := False;
  ARB_occlusion_query := False;
  ARB_occlusion_query2 := False;
  ARB_pixel_buffer_object := False;
  ARB_point_parameters := False;
  ARB_point_sprite := False;
  ARB_provoking_vertex := False;
  ARB_sample_shading := False;
  ARB_sampler_objects := False;
  ARB_seamless_cube_map := False;
  ARB_shader_bit_encoding := False;
  ARB_shader_objects := False;
  ARB_shader_subroutine := False;
  ARB_shader_texture_lod := False;
  ARB_shading_language_100 := False;
  ARB_shadow := False;
  ARB_shadow_ambient := False;
  ARB_sync := False;
  ARB_tessellation_shader := False;
  ARB_texture_border_clamp := False;
  ARB_texture_buffer_object := False;
  ARB_texture_buffer_object_rgb32 := False;
  ARB_texture_compression := False;
  ARB_texture_compression_rgtc := False;
  ARB_texture_cube_map := False;
  ARB_texture_cube_map_array := False;
  ARB_texture_env_add := False;
  ARB_texture_env_combine := False;
  ARB_texture_env_crossbar := False;
  ARB_texture_env_dot3 := False;
  ARB_texture_float := False;
  ARB_texture_gather := False;
  ARB_texture_mirrored_repeat := False;
  ARB_texture_multisample := False;
  ARB_texture_non_power_of_two := False;
  ARB_texture_query_lod := False;
  ARB_texture_rectangle := False;
  ARB_texture_rg := False;
  ARB_texture_rgb10_a2ui := False;
  ARB_texture_swizzle := False;
  ARB_timer_query := False;
  ARB_transform_feedback2 := False;
  ARB_transform_feedback3 := False;
  ARB_transpose_matrix := False;
  ARB_uniform_buffer_object := False;
  ARB_vertex_array_bgra := False;
  ARB_vertex_array_object := False;
  ARB_vertex_blend := False;
  ARB_vertex_buffer_object := False;
  ARB_vertex_program := False;
  ARB_vertex_shader := False;
  ARB_vertex_type_2_10_10_10_rev := False;
  ARB_window_pos := False;
  ARB_texture_compression_bptc := False;
  ARB_get_program_binary := False;
  ARB_separate_shader_objects := False;

  // check Vendor/EXT OpenGL extensions
  _3DFX_multisample := False;
  _3DFX_tbuffer := False;
  _3DFX_texture_compression_FXT1 := False;
  ATI_draw_buffers := False;
  ATI_texture_compression_3dc := False;
  ATI_texture_float := False;
  ATI_texture_mirror_once := False;

  S3_s3tc := False;

  EXT_abgr := False;
  EXT_bgra := False;
  EXT_bindable_uniform := False;
  EXT_blend_color := False;
  EXT_blend_equation_separate := False;
  EXT_blend_func_separate := False;
  EXT_blend_logic_op := False;
  EXT_blend_minmax := False;
  EXT_blend_subtract := False;
  EXT_Cg_shader := False;
  EXT_clip_volume_hint := False;
  EXT_compiled_vertex_array := False;
  EXT_copy_texture := False;
  EXT_depth_bounds_test := False;
  EXT_draw_buffers2 := False;
  EXT_draw_instanced := False;
  EXT_draw_range_elements := False;
  EXT_fog_coord := False;
  EXT_framebuffer_blit := False;
  EXT_framebuffer_multisample := False;
  EXT_framebuffer_object := False;
  EXT_framebuffer_sRGB := False;
  EXT_geometry_shader4 := False;
  EXT_gpu_program_parameters := False;
  EXT_gpu_shader4 := False;
  EXT_multi_draw_arrays := False;
  EXT_multisample := False;
  EXT_packed_depth_stencil := False;
  EXT_packed_float := False;
  EXT_packed_pixels := False;
  EXT_paletted_texture := False;
  EXT_pixel_buffer_object := False;
  EXT_polygon_offset := False;
  EXT_rescale_normal := False;
  EXT_secondary_color := False;
  EXT_separate_specular_color := False;
  EXT_shadow_funcs := False;
  EXT_shared_texture_palette := False;
  EXT_stencil_clear_tag := False;
  EXT_stencil_two_side := False;
  EXT_stencil_wrap := False;
  EXT_texture3D := False;
  EXT_texture_array := False;
  EXT_texture_buffer_object := False;
  EXT_texture_compression_latc := False;
  EXT_texture_compression_rgtc := False;
  EXT_texture_compression_s3tc := False;
  EXT_texture_cube_map := False;
  EXT_texture_edge_clamp := False;
  EXT_texture_env_add := False;
  EXT_texture_env_combine := False;
  EXT_texture_env_dot3 := False;
  EXT_texture_filter_anisotropic := False;
  EXT_texture_integer := False;
  EXT_texture_lod := False;
  EXT_texture_lod_bias := False;
  EXT_texture_mirror_clamp := False;
  EXT_texture_object := False;
  EXT_texture_rectangle := False;
  EXT_texture_sRGB := False;
  EXT_texture_shared_exponent := False;
  EXT_timer_query := False;
  EXT_transform_feedback := False;
  EXT_vertex_array := False;
  EXT_texture_sRGB_decode := False;
  EXT_direct_state_access := False;
  EXT_texture_swizzle := False;

  HP_occlusion_test := False;

  IBM_rasterpos_clip := False;

  KTX_buffer_region := False;

  MESA_resize_buffers := False;

  NV_blend_square := False;
  NV_conditional_render := False;
  NV_copy_image := False;
  NV_depth_buffer_float := False;
  NV_fence := False;
  NV_float_buffer := False;
  NV_fog_distance := False;
  NV_geometry_program4 := False;
  NV_light_max_exponent := False;
  NV_multisample_filter_hint := False;
  NV_occlusion_query := False;
  NV_point_sprite := False;
  NV_primitive_restart := False;
  NV_register_combiners := False;
  NV_shader_buffer_load := False;
  NV_texgen_reflection := False;
  NV_texture_compression_vtc := False;
  NV_texture_env_combine4 := False;
  NV_texture_rectangle := False;
  NV_texture_shader := False;
  NV_texture_shader2 := False;
  NV_texture_shader3 := False;
  NV_transform_feedback := False;
  NV_vertex_array_range := False;
  NV_vertex_array_range2 := False;
  NV_vertex_buffer_unified_memory := False;
  NV_vertex_program := False;

  SGI_color_matrix := False;

  SGIS_generate_mipmap := False;
  SGIS_multisample := False;
  SGIS_texture_border_clamp := False;
  SGIS_texture_color_mask := False;
  SGIS_texture_edge_clamp := False;
  SGIS_texture_lod := False;

  SGIX_depth_texture := False;
  SGIX_shadow := False;
  SGIX_shadow_ambient := False;

  AMD_vertex_shader_tessellator := False;

  WIN_swap_hint := False;
  ATI_meminfo := False;
  NVX_gpu_memory_info := False;
  NV_vdpau_interop := False;
  NV_path_rendering := False;

  GREMEDY_frame_terminator := False;
  GREMEDY_string_marker := False;
  ARB_debug_output := False;

  BindTexture := @glCapBindTexture;
  BlendFunc := @glCapBlendFunc;
  Clear := @glCapClear;
  ClearColor := @glCapClearColor;
  ClearDepth := @glCapClearDepth;
  ClearStencil := @glCapClearStencil;
  ColorMask := @glCapColorMask;
  CopyTexImage1D := @glCapCopyTexImage1D;
  CopyTexImage2D := @glCapCopyTexImage2D;
  CopyTexSubImage1D := @glCapCopyTexSubImage1D;
  CopyTexSubImage2D := @glCapCopyTexSubImage2D;
  CullFace := @glCapCullFace;
  DeleteTextures := @glCapDeleteTextures;
  DepthFunc := @glCapDepthFunc;
  DepthMask := @glCapDepthMask;
  DepthRange := @glCapDepthRange;
  Disable := @glCapDisable;
  DrawArrays := @glCapDrawArrays;
  DrawBuffer := @glCapDrawBuffer;
  DrawElements := @glCapDrawElements;
  Enable := @glCapEnable;
  Finish := @glCapFinish;
  Flush := @glCapFlush;
  FrontFace := @glCapFrontFace;
  GenTextures := @glCapGenTextures;
  GetBooleanv := @glCapGetBooleanv;
  GetDoublev := @glCapGetDoublev;
  GetError := @glCapGetError;
  GetFloatv := @glCapGetFloatv;
  GetPointerv := @glCapGetPointerv;
  GetString := @glCapGetString;
  GetTexImage := @glCapGetTexImage;
  GetTexLevelParameterfv := @glCapGetTexLevelParameterfv;
  GetTexLevelParameteriv := @glCapGetTexLevelParameteriv;
  GetTexParameterfv := @glCapGetTexParameterfv;
  GetTexParameteriv := @glCapGetTexParameteriv;
  Hint := @glCapHint;
  IsEnabled := @glCapIsEnabled;
  IsTexture := @glCapIsTexture;
  LineWidth := @glCapLineWidth;
  LogicOp := @glCapLogicOp;
  PixelStoref := @glCapPixelStoref;
  PixelStorei := @glCapPixelStorei;
  PointSize := @glCapPointSize;
  PolygonMode := @glCapPolygonMode;
  PolygonOffset := @glCapPolygonOffset;
  ReadBuffer := @glCapReadBuffer;
  ReadPixels := @glCapReadPixels;
  Scissor := @glCapScissor;
  StencilFunc := @glCapStencilFunc;
  StencilMask := @glCapStencilMask;
  StencilOp := @glCapStencilOp;
  TexImage1D := @glCapTexImage1D;
  TexImage2D := @glCapTexImage2D;
  TexParameterf := @glCapTexParameterf;
  TexParameterfv := @glCapTexParameterfv;
  TexParameteri := @glCapTexParameteri;
  TexParameteriv := @glCapTexParameteriv;
  TexSubImage1D := @glCapTexSubImage1D;
  TexSubImage2D := @glCapTexSubImage2D;
  Viewport := @glCapViewport;
  Accum := @glCapAccum;
  AlphaFunc := @glCapAlphaFunc;
  AreTexturesResident := @glCapAreTexturesResident;
  ArrayElement := @glCapArrayElement;
  Begin_ := @glCapBegin_;
  Bitmap := @glCapBitmap;
  CallList := @glCapCallList;
  CallLists := @glCapCallLists;
  ClearAccum := @glCapClearAccum;
  ClearIndex := @glCapClearIndex;
  ClipPlane := @glCapClipPlane;
  Color3b := @glCapColor3b;
  Color3bv := @glCapColor3bv;
  Color3d := @glCapColor3d;
  Color3dv := @glCapColor3dv;
  Color3f := @glCapColor3f;
  Color3fv := @glCapColor3fv;
  Color3i := @glCapColor3i;
  Color3iv := @glCapColor3iv;
  Color3s := @glCapColor3s;
  Color3sv := @glCapColor3sv;
  Color3ub := @glCapColor3ub;
  Color3ubv := @glCapColor3ubv;
  Color3ui := @glCapColor3ui;
  Color3uiv := @glCapColor3uiv;
  Color3us := @glCapColor3us;
  Color3usv := @glCapColor3usv;
  Color4b := @glCapColor4b;
  Color4bv := @glCapColor4bv;
  Color4d := @glCapColor4d;
  Color4dv := @glCapColor4dv;
  Color4f := @glCapColor4f;
  Color4fv := @glCapColor4fv;
  Color4i := @glCapColor4i;
  Color4iv := @glCapColor4iv;
  Color4s := @glCapColor4s;
  Color4sv := @glCapColor4sv;
  Color4ub := @glCapColor4ub;
  Color4ubv := @glCapColor4ubv;
  Color4ui := @glCapColor4ui;
  Color4uiv := @glCapColor4uiv;
  Color4us := @glCapColor4us;
  Color4usv := @glCapColor4usv;
  ColorMaterial := @glCapColorMaterial;
  ColorPointer := @glCapColorPointer;
  CopyPixels := @glCapCopyPixels;
  DeleteLists := @glCapDeleteLists;
  DisableClientState := @glCapDisableClientState;
  DrawPixels := @glCapDrawPixels;
  EdgeFlag := @glCapEdgeFlag;
  EdgeFlagPointer := @glCapEdgeFlagPointer;
  EdgeFlagv := @glCapEdgeFlagv;
  EnableClientState := @glCapEnableClientState;
  End_ := @glCapEnd_;
  EndList := @glCapEndList;
  EvalCoord1d := @glCapEvalCoord1d;
  EvalCoord1dv := @glCapEvalCoord1dv;
  EvalCoord1f := @glCapEvalCoord1f;
  EvalCoord1fv := @glCapEvalCoord1fv;
  EvalCoord2d := @glCapEvalCoord2d;
  EvalCoord2dv := @glCapEvalCoord2dv;
  EvalCoord2f := @glCapEvalCoord2f;
  EvalCoord2fv := @glCapEvalCoord2fv;
  EvalMesh1 := @glCapEvalMesh1;
  EvalMesh2 := @glCapEvalMesh2;
  EvalPoint1 := @glCapEvalPoint1;
  EvalPoint2 := @glCapEvalPoint2;
  FeedbackBuffer := @glCapFeedbackBuffer;
  Fogf := @glCapFogf;
  Fogfv := @glCapFogfv;
  Fogi := @glCapFogi;
  Fogiv := @glCapFogiv;
  Frustum := @glCapFrustum;
  GenLists := @glCapGenLists;
  GetClipPlane := @glCapGetClipPlane;
  GetLightfv := @glCapGetLightfv;
  GetLightiv := @glCapGetLightiv;
  GetMapdv := @glCapGetMapdv;
  GetMapfv := @glCapGetMapfv;
  GetMapiv := @glCapGetMapiv;
  GetMaterialfv := @glCapGetMaterialfv;
  GetMaterialiv := @glCapGetMaterialiv;
  GetPixelMapfv := @glCapGetPixelMapfv;
  GetPixelMapuiv := @glCapGetPixelMapuiv;
  GetPixelMapusv := @glCapGetPixelMapusv;
  GetPolygonStipple := @glCapGetPolygonStipple;
  GetTexEnvfv := @glCapGetTexEnvfv;
  GetTexEnviv := @glCapGetTexEnviv;
  GetTexGendv := @glCapGetTexGendv;
  GetTexGenfv := @glCapGetTexGenfv;
  GetTexGeniv := @glCapGetTexGeniv;
  IndexMask := @glCapIndexMask;
  IndexPointer := @glCapIndexPointer;
  Indexd := @glCapIndexd;
  Indexdv := @glCapIndexdv;
  Indexf := @glCapIndexf;
  Indexfv := @glCapIndexfv;
  Indexi := @glCapIndexi;
  Indexiv := @glCapIndexiv;
  Indexs := @glCapIndexs;
  Indexsv := @glCapIndexsv;
  Indexub := @glCapIndexub;
  Indexubv := @glCapIndexubv;
  InitNames := @glCapInitNames;
  InterleavedArrays := @glCapInterleavedArrays;
  IsList := @glCapIsList;
  LightModelf := @glCapLightModelf;
  LightModelfv := @glCapLightModelfv;
  LightModeli := @glCapLightModeli;
  LightModeliv := @glCapLightModeliv;
  Lightf := @glCapLightf;
  Lightfv := @glCapLightfv;
  Lighti := @glCapLighti;
  Lightiv := @glCapLightiv;
  LineStipple := @glCapLineStipple;
  ListBase := @glCapListBase;
  LoadIdentity := @glCapLoadIdentity;
  LoadMatrixd := @glCapLoadMatrixd;
  LoadMatrixf := @glCapLoadMatrixf;
  LoadName := @glCapLoadName;
  Map1d := @glCapMap1d;
  Map1f := @glCapMap1f;
  Map2d := @glCapMap2d;
  Map2f := @glCapMap2f;
  MapGrid1d := @glCapMapGrid1d;
  MapGrid1f := @glCapMapGrid1f;
  MapGrid2d := @glCapMapGrid2d;
  MapGrid2f := @glCapMapGrid2f;
  Materialf := @glCapMaterialf;
  Materialfv := @glCapMaterialfv;
  Materiali := @glCapMateriali;
  Materialiv := @glCapMaterialiv;
  MatrixMode := @glCapMatrixMode;
  MultMatrixd := @glCapMultMatrixd;
  MultMatrixf := @glCapMultMatrixf;
  NewList := @glCapNewList;
  Normal3b := @glCapNormal3b;
  Normal3bv := @glCapNormal3bv;
  Normal3d := @glCapNormal3d;
  Normal3dv := @glCapNormal3dv;
  Normal3f := @glCapNormal3f;
  Normal3fv := @glCapNormal3fv;
  Normal3i := @glCapNormal3i;
  Normal3iv := @glCapNormal3iv;
  Normal3s := @glCapNormal3s;
  Normal3sv := @glCapNormal3sv;
  NormalPointer := @glCapNormalPointer;
  Ortho := @glCapOrtho;
  PassThrough := @glCapPassThrough;
  PixelMapfv := @glCapPixelMapfv;
  PixelMapuiv := @glCapPixelMapuiv;
  PixelMapusv := @glCapPixelMapusv;
  PixelTransferf := @glCapPixelTransferf;
  PixelTransferi := @glCapPixelTransferi;
  PixelZoom := @glCapPixelZoom;
  PolygonStipple := @glCapPolygonStipple;
  PopAttrib := @glCapPopAttrib;
  PopClientAttrib := @glCapPopClientAttrib;
  PopMatrix := @glCapPopMatrix;
  PopName := @glCapPopName;
  PrioritizeTextures := @glCapPrioritizeTextures;
  PushAttrib := @glCapPushAttrib;
  PushClientAttrib := @glCapPushClientAttrib;
  PushMatrix := @glCapPushMatrix;
  PushName := @glCapPushName;
  RasterPos2d := @glCapRasterPos2d;
  RasterPos2dv := @glCapRasterPos2dv;
  RasterPos2f := @glCapRasterPos2f;
  RasterPos2fv := @glCapRasterPos2fv;
  RasterPos2i := @glCapRasterPos2i;
  RasterPos2iv := @glCapRasterPos2iv;
  RasterPos2s := @glCapRasterPos2s;
  RasterPos2sv := @glCapRasterPos2sv;
  RasterPos3d := @glCapRasterPos3d;
  RasterPos3dv := @glCapRasterPos3dv;
  RasterPos3f := @glCapRasterPos3f;
  RasterPos3fv := @glCapRasterPos3fv;
  RasterPos3i := @glCapRasterPos3i;
  RasterPos3iv := @glCapRasterPos3iv;
  RasterPos3s := @glCapRasterPos3s;
  RasterPos3sv := @glCapRasterPos3sv;
  RasterPos4d := @glCapRasterPos4d;
  RasterPos4dv := @glCapRasterPos4dv;
  RasterPos4f := @glCapRasterPos4f;
  RasterPos4fv := @glCapRasterPos4fv;
  RasterPos4i := @glCapRasterPos4i;
  RasterPos4iv := @glCapRasterPos4iv;
  RasterPos4s := @glCapRasterPos4s;
  RasterPos4sv := @glCapRasterPos4sv;
  Rectd := @glCapRectd;
  Rectdv := @glCapRectdv;
  Rectf := @glCapRectf;
  Rectfv := @glCapRectfv;
  Recti := @glCapRecti;
  Rectiv := @glCapRectiv;
  Rects := @glCapRects;
  Rectsv := @glCapRectsv;
  RenderMode := @glCapRenderMode;
  Rotated := @glCapRotated;
  Rotatef := @glCapRotatef;
  Scaled := @glCapScaled;
  Scalef := @glCapScalef;
  SelectBuffer := @glCapSelectBuffer;
  ShadeModel := @glCapShadeModel;
  TexCoord1d := @glCapTexCoord1d;
  TexCoord1dv := @glCapTexCoord1dv;
  TexCoord1f := @glCapTexCoord1f;
  TexCoord1fv := @glCapTexCoord1fv;
  TexCoord1i := @glCapTexCoord1i;
  TexCoord1iv := @glCapTexCoord1iv;
  TexCoord1s := @glCapTexCoord1s;
  TexCoord1sv := @glCapTexCoord1sv;
  TexCoord2d := @glCapTexCoord2d;
  TexCoord2dv := @glCapTexCoord2dv;
  TexCoord2f := @glCapTexCoord2f;
  TexCoord2fv := @glCapTexCoord2fv;
  TexCoord2i := @glCapTexCoord2i;
  TexCoord2iv := @glCapTexCoord2iv;
  TexCoord2s := @glCapTexCoord2s;
  TexCoord2sv := @glCapTexCoord2sv;
  TexCoord3d := @glCapTexCoord3d;
  TexCoord3dv := @glCapTexCoord3dv;
  TexCoord3f := @glCapTexCoord3f;
  TexCoord3fv := @glCapTexCoord3fv;
  TexCoord3i := @glCapTexCoord3i;
  TexCoord3iv := @glCapTexCoord3iv;
  TexCoord3s := @glCapTexCoord3s;
  TexCoord3sv := @glCapTexCoord3sv;
  TexCoord4d := @glCapTexCoord4d;
  TexCoord4dv := @glCapTexCoord4dv;
  TexCoord4f := @glCapTexCoord4f;
  TexCoord4fv := @glCapTexCoord4fv;
  TexCoord4i := @glCapTexCoord4i;
  TexCoord4iv := @glCapTexCoord4iv;
  TexCoord4s := @glCapTexCoord4s;
  TexCoord4sv := @glCapTexCoord4sv;
  TexCoordPointer := @glCapTexCoordPointer;
  TexEnvf := @glCapTexEnvf;
  TexEnvfv := @glCapTexEnvfv;
  TexEnvi := @glCapTexEnvi;
  TexEnviv := @glCapTexEnviv;
  TexGend := @glCapTexGend;
  TexGendv := @glCapTexGendv;
  TexGenf := @glCapTexGenf;
  TexGenfv := @glCapTexGenfv;
  TexGeni := @glCapTexGeni;
  TexGeniv := @glCapTexGeniv;
  Translated := @glCapTranslated;
  Translatef := @glCapTranslatef;
  Vertex2d := @glCapVertex2d;
  Vertex2dv := @glCapVertex2dv;
  Vertex2f := @glCapVertex2f;
  Vertex2fv := @glCapVertex2fv;
  Vertex2i := @glCapVertex2i;
  Vertex2iv := @glCapVertex2iv;
  Vertex2s := @glCapVertex2s;
  Vertex2sv := @glCapVertex2sv;
  Vertex3d := @glCapVertex3d;
  Vertex3dv := @glCapVertex3dv;
  Vertex3f := @glCapVertex3f;
  Vertex3fv := @glCapVertex3fv;
  Vertex3i := @glCapVertex3i;
  Vertex3iv := @glCapVertex3iv;
  Vertex3s := @glCapVertex3s;
  Vertex3sv := @glCapVertex3sv;
  Vertex4d := @glCapVertex4d;
  Vertex4dv := @glCapVertex4dv;
  Vertex4f := @glCapVertex4f;
  Vertex4fv := @glCapVertex4fv;
  Vertex4i := @glCapVertex4i;
  Vertex4iv := @glCapVertex4iv;
  Vertex4s := @glCapVertex4s;
  Vertex4sv := @glCapVertex4sv;
  VertexPointer := @glCapVertexPointer;
  BlendColor := @glCapBlendColor;
  BlendEquation := @glCapBlendEquation;
  DrawRangeElements := @glCapDrawRangeElements;
  TexImage3D := @glCapTexImage3D;
  TexSubImage3D := @glCapTexSubImage3D;
  CopyTexSubImage3D := @glCapCopyTexSubImage3D;

  IsRenderbuffer := @glCapIsRenderbuffer;
  BindRenderbuffer := @glCapBindRenderbuffer;
  DeleteRenderbuffers := @glCapDeleteRenderbuffers;
  GenRenderbuffers := @glCapGenRenderbuffers;
  RenderbufferStorage := @glCapRenderbufferStorage;
  RenderbufferStorageMultisample := @glCapRenderbufferStorageMultisample;
  GetRenderbufferParameteriv := @glCapGetRenderbufferParameteriv;
  IsFramebuffer := @glCapIsFramebuffer;
  BindFramebuffer := @glCapBindFramebuffer;
  DeleteFramebuffers := @glCapDeleteFramebuffers;
  GenFramebuffers := @glCapGenFramebuffers;
  CheckFramebufferStatus := @glCapCheckFramebufferStatus;
  FramebufferTexture := @glCapFramebufferTexture;
  FramebufferTexture1D := @glCapFramebufferTexture1D;
  FramebufferTexture2D := @glCapFramebufferTexture2D;
  FramebufferTexture3D := @glCapFramebufferTexture3D;
  FramebufferTextureLayer := @glCapFramebufferTextureLayer;
  FramebufferTextureFace := @glCapFramebufferTextureFace;
  FramebufferRenderbuffer := @glCapFramebufferRenderbuffer;
  GetFramebufferAttachmentParameteriv := @glCapGetFramebufferAttachmentParameteriv;
  BlitFramebuffer := @glCapBlitFramebuffer;
  GenerateMipmap := @glCapGenerateMipmap;
  ClearBufferiv := @glCapClearBufferiv;
  ClearBufferuiv := @glCapClearBufferuiv;
  ClearBufferfv := @glCapClearBufferfv;
  ClearBufferfi := @glCapClearBufferfi;
  LockArrays := @glCapLockArrays;
  UnlockArrays := @glCapUnlockArrays;
  BindBuffer := @glCapBindBuffer;
  DeleteBuffers := @glCapDeleteBuffers;
  GenBuffers := @glCapGenBuffers;
  IsBuffer := @glCapIsBuffer;
  BufferData := @glCapBufferData;
  BufferSubData := @glCapBufferSubData;
  GetBufferSubData := @glCapGetBufferSubData;
  MapBuffer := @glCapMapBuffer;
  UnmapBuffer := @glCapUnmapBuffer;
  GetBufferParameteriv := @glCapGetBufferParameteriv;
  GetBufferPointerv := @glCapGetBufferPointerv;
  MapBufferRange := @glCapMapBufferRange;
  FlushMappedBufferRange := @glCapFlushMappedBufferRange;
  BindBufferRange := @glCapBindBufferRange;
  BindBufferOffset := @glCapBindBufferOffset;
  BindBufferBase := @glCapBindBufferBase;
  BeginTransformFeedback := @glCapBeginTransformFeedback;
  EndTransformFeedback := @glCapEndTransformFeedback;
  TransformFeedbackVaryings := @glCapTransformFeedbackVaryings;
  GetTransformFeedbackVarying := @glCapGetTransformFeedbackVarying;

  TransformFeedbackAttribs := @glCapTransformFeedbackAttribs;
  TransformFeedbackVaryingsNV := @glCapTransformFeedbackVaryingsNV;
  TexBuffer := @glCapTexBuffer;
  BindVertexArray := @glCapBindVertexArray;
  DeleteVertexArrays := @glCapDeleteVertexArrays;
  GenVertexArrays := @glCapGenVertexArrays;
  IsVertexArray := @glCapIsVertexArray;
  FlushVertexArrayRangeNV := @glCapFlushVertexArrayRangeNV;
  VertexArrayRangeNV := @glCapVertexArrayRangeNV;
  CopyBufferSubData := @glCapCopyBufferSubData;
  UniformBuffer := @glCapUniformBuffer;
  GetUniformBufferSize := @glCapGetUniformBufferSize;
  GetUniformOffset := @glCapGetUniformOffset;
  PrimitiveRestartIndex := @glCapPrimitiveRestartIndex;

  DrawElementsBaseVertex := @glCapDrawElementsBaseVertex;
  DrawRangeElementsBaseVertex := @glCapDrawRangeElementsBaseVertex;
  DrawElementsInstancedBaseVertex := @glCapDrawElementsInstancedBaseVertex;
  MultiDrawElementsBaseVertex := @glCapMultiDrawElementsBaseVertex;
  DrawArraysInstanced := @glCapDrawArraysInstanced;
  DrawElementsInstanced := @glCapDrawElementsInstanced;

  VertexAttrib1d := @glCapVertexAttrib1d;
  VertexAttrib1dv := @glCapVertexAttrib1dv;
  VertexAttrib1f := @glCapVertexAttrib1f;
  VertexAttrib1fv := @glCapVertexAttrib1fv;
  VertexAttrib1s := @glCapVertexAttrib1s;
  VertexAttrib1sv := @glCapVertexAttrib1sv;
  VertexAttrib2d := @glCapVertexAttrib2d;
  VertexAttrib2dv := @glCapVertexAttrib2dv;
  VertexAttrib2f := @glCapVertexAttrib2f;
  VertexAttrib2fv := @glCapVertexAttrib2fv;
  VertexAttrib2s := @glCapVertexAttrib2s;
  VertexAttrib2sv := @glCapVertexAttrib2sv;
  VertexAttrib3d := @glCapVertexAttrib3d;
  VertexAttrib3dv := @glCapVertexAttrib3dv;
  VertexAttrib3f := @glCapVertexAttrib3f;
  VertexAttrib3fv := @glCapVertexAttrib3fv;
  VertexAttrib3s := @glCapVertexAttrib3s;
  VertexAttrib3sv := @glCapVertexAttrib3sv;
  VertexAttrib4Nbv := @glCapVertexAttrib4Nbv;
  VertexAttrib4Niv := @glCapVertexAttrib4Niv;
  VertexAttrib4Nsv := @glCapVertexAttrib4Nsv;
  VertexAttrib4Nub := @glCapVertexAttrib4Nub;
  VertexAttrib4Nubv := @glCapVertexAttrib4Nubv;
  VertexAttrib4Nuiv := @glCapVertexAttrib4Nuiv;
  VertexAttrib4Nusv := @glCapVertexAttrib4Nusv;
  VertexAttrib4bv := @glCapVertexAttrib4bv;
  VertexAttrib4d := @glCapVertexAttrib4d;
  VertexAttrib4dv := @glCapVertexAttrib4dv;
  VertexAttrib4f := @glCapVertexAttrib4f;
  VertexAttrib4fv := @glCapVertexAttrib4fv;
  VertexAttrib4iv := @glCapVertexAttrib4iv;
  VertexAttrib4s := @glCapVertexAttrib4s;
  VertexAttrib4sv := @glCapVertexAttrib4sv;
  VertexAttrib4ubv := @glCapVertexAttrib4ubv;
  VertexAttrib4uiv := @glCapVertexAttrib4uiv;
  VertexAttrib4usv := @glCapVertexAttrib4usv;
  VertexAttribPointer := @glCapVertexAttribPointer;
  VertexAttribI1i := @glCapVertexAttribI1i;
  VertexAttribI2i := @glCapVertexAttribI2i;
  VertexAttribI3i := @glCapVertexAttribI3i;
  VertexAttribI4i := @glCapVertexAttribI4i;
  VertexAttribI1ui := @glCapVertexAttribI1ui;
  VertexAttribI2ui := @glCapVertexAttribI2ui;
  VertexAttribI3ui := @glCapVertexAttribI3ui;
  VertexAttribI4ui := @glCapVertexAttribI4ui;
  VertexAttribI1iv := @glCapVertexAttribI1iv;
  VertexAttribI2iv := @glCapVertexAttribI2iv;
  VertexAttribI3iv := @glCapVertexAttribI3iv;
  VertexAttribI4iv := @glCapVertexAttribI4iv;
  VertexAttribI1uiv := @glCapVertexAttribI1uiv;
  VertexAttribI2uiv := @glCapVertexAttribI2uiv;
  VertexAttribI3uiv := @glCapVertexAttribI3uiv;
  VertexAttribI4uiv := @glCapVertexAttribI4uiv;
  VertexAttribI4bv := @glCapVertexAttribI4bv;
  VertexAttribI4sv := @glCapVertexAttribI4sv;
  VertexAttribI4ubv := @glCapVertexAttribI4ubv;
  VertexAttribI4usv := @glCapVertexAttribI4usv;
  VertexAttribIPointer := @glCapVertexAttribIPointer;
  GetVertexAttribIiv := @glCapGetVertexAttribIiv;
  GetVertexAttribIuiv := @glCapGetVertexAttribIuiv;
  EnableVertexAttribArray := @glCapEnableVertexAttribArray;
  DisableVertexAttribArray := @glCapDisableVertexAttribArray;
  VertexAttribDivisor := @glCapVertexAttribDivisor;

  GenQueries := @glCapGenQueries;
  DeleteQueries := @glCapDeleteQueries;
  IsQuery := @glCapIsQuery;
  BeginQuery := @glCapBeginQuery;
  EndQuery := @glCapEndQuery;
  GetQueryiv := @glCapGetQueryiv;
  GetQueryObjectiv := @glCapGetQueryObjectiv;
  GetQueryObjectuiv := @glCapGetQueryObjectuiv;
  QueryCounter := @glCapQueryCounter;
  GetQueryObjecti64v := @glCapGetQueryObjecti64v;
  GetQueryObjectui64v := @glCapGetQueryObjectui64v;

  DeleteObject := @glCapDeleteObject;
{$IFDEF GLS_OPENGL_ES}
  DeleteShader := @glCapDeleteShader;
  DeleteProgram := @glCapDeleteProgram;
{$ENDIF}
  GetHandle := @glCapGetHandle;
  DetachShader := @glCapDetachShader;
  CreateShader := @glCapCreateShader;
  ShaderSource := @glCapShaderSource;
  CompileShader := @glCapCompileShader;
  CreateProgram := @glCapCreateProgram;
  AttachShader := @glCapAttachShader;
  LinkProgram := @glCapLinkProgram;
  UseProgram := @glCapUseProgram;
  ValidateProgram := @glCapValidateProgram;
  Uniform1f := @glCapUniform1f;
  Uniform2f := @glCapUniform2f;
  Uniform3f := @glCapUniform3f;
  Uniform4f := @glCapUniform4f;
  Uniform1i := @glCapUniform1i;
  Uniform2i := @glCapUniform2i;
  Uniform3i := @glCapUniform3i;
  Uniform4i := @glCapUniform4i;
  Uniform1fv := @glCapUniform1fv;
  Uniform2fv := @glCapUniform2fv;
  Uniform3fv := @glCapUniform3fv;
  Uniform4fv := @glCapUniform4fv;
  Uniform1iv := @glCapUniform1iv;
  Uniform2iv := @glCapUniform2iv;
  Uniform3iv := @glCapUniform3iv;
  Uniform4iv := @glCapUniform4iv;
  Uniform1ui := @glCapUniform1ui;
  Uniform2ui := @glCapUniform2ui;
  Uniform3ui := @glCapUniform3ui;
  Uniform4ui := @glCapUniform4ui;
  Uniform1uiv := @glCapUniform1uiv;
  Uniform2uiv := @glCapUniform2uiv;
  Uniform3uiv := @glCapUniform3uiv;
  Uniform4uiv := @glCapUniform4uiv;
  GetUniformuiv := @glCapGetUniformuiv;
  UniformMatrix2fv := @glCapUniformMatrix2fv;
  UniformMatrix3fv := @glCapUniformMatrix3fv;
  UniformMatrix4fv := @glCapUniformMatrix4fv;
  BindFragDataLocation := @glCapBindFragDataLocation;
  GetFragDataLocation := @glCapGetFragDataLocation;
  ClampColor := @glCapClampColor;
  ColorMaski := @glCapColorMaski;
  GetBooleani_v := @glCapGetBooleani_v;
  GetIntegeri_v := @glCapGetIntegeri_v;
  Enablei := @glCapEnablei;
  Disablei := @glCapDisablei;
  IsEnabledi := @glCapIsEnabledi;
  BindFragDataLocationIndexed := @glCapBindFragDataLocationIndexed;
  GetFragDataIndex := @glCapGetFragDataIndex;
  GetObjectParameterfv := @glCapGetObjectParameterfv;
  GetObjectParameteriv := @glCapGetObjectParameteriv;
  GetAttachedObjects := @glCapGetAttachedObjects;
  GetActiveAttrib := @glCapGetActiveAttrib;
  GetActiveUniform := @glCapGetActiveUniform;
  GetAttachedShaders := @glCapGetAttachedShaders;
  GetAttribLocation := @glCapGetAttribLocation;
  GetProgramiv := @glCapGetProgramiv;
  GetProgramInfoLog := @glCapGetProgramInfoLog;
  GetShaderiv := @glCapGetShaderiv;
  GetInfoLog := @glCapGetInfoLog;
  GetShaderInfoLog := @glCapGetShaderInfoLog;
  GetShaderSource := @glCapGetShaderSource;
  GetUniformLocation := @glCapGetUniformLocation;
  GetUniformfv := @glCapGetUniformfv;
  GetUniformiv := @glCapGetUniformiv;
  GetVertexAttribdv := @glCapGetVertexAttribdv;
  GetVertexAttribfv := @glCapGetVertexAttribfv;
  GetVertexAttribiv := @glCapGetVertexAttribiv;
  GetVertexAttribPointerv := @glCapGetVertexAttribPointerv;
  IsProgram := @glCapIsProgram;
  IsShader := @glCapIsShader;
  GetUniformLocation := @glCapGetUniformLocation;
  BindAttribLocation := @glCapBindAttribLocation;
  GetVaryingLocation := @glCapGetVaryingLocation;
  GetActiveVarying := @glCapGetActiveVarying;
  ActiveVarying := @glCapActiveVarying;
  GetUniformIndices := @glCapGetUniformIndices;
  GetActiveUniformsiv := @glCapGetActiveUniformsiv;
  GetActiveUniformName := @glCapGetActiveUniformName;
  GetUniformBlockIndex := @glCapGetUniformBlockIndex;
  GetActiveUniformBlockiv := @glCapGetActiveUniformBlockiv;
  GetActiveUniformBlockName := @glCapGetActiveUniformBlockName;
  UniformBlockBinding := @glCapUniformBlockBinding;
  GetProgramBinary := @glCapGetProgramBinary;
  ProgramBinary := @glCapProgramBinary;
  UseProgramStages := @glCapUseProgramStages;
  ActiveShaderProgram := @glCapActiveShaderProgram;
  CreateShaderProgramv := @glCapCreateShaderProgramv;
  BindProgramPipeline := @glCapBindProgramPipeline;
  DeleteProgramPipelines := @glCapDeleteProgramPipelines;
  GenProgramPipelines := @glCapGenProgramPipelines;
  IsProgramPipeline := @glCapIsProgramPipeline;
  GetProgramPipelineiv := @glCapGetProgramPipelineiv;
  ProgramUniform1i := @glCapProgramUniform1i;
  ProgramUniform1iv := @glCapProgramUniform1iv;
  ProgramUniform1f := @glCapProgramUniform1f;
  ProgramUniform1fv := @glCapProgramUniform1fv;
  ProgramUniform1d := @glCapProgramUniform1d;
  ProgramUniform1dv := @glCapProgramUniform1dv;
  ProgramUniform1ui := @glCapProgramUniform1ui;
  ProgramUniform1uiv := @glCapProgramUniform1uiv;
  ProgramUniform2i := @glCapProgramUniform2i;
  ProgramUniform2iv := @glCapProgramUniform2iv;
  ProgramUniform2f := @glCapProgramUniform2f;
  ProgramUniform2fv := @glCapProgramUniform2fv;
  ProgramUniform2d := @glCapProgramUniform2d;
  ProgramUniform2dv := @glCapProgramUniform2dv;
  ProgramUniform2ui := @glCapProgramUniform2ui;
  ProgramUniform2uiv := @glCapProgramUniform2uiv;
  ProgramUniform3i := @glCapProgramUniform3i;
  ProgramUniform3iv := @glCapProgramUniform3iv;
  ProgramUniform3f := @glCapProgramUniform3f;
  ProgramUniform3fv := @glCapProgramUniform3fv;
  ProgramUniform3d := @glCapProgramUniform3d;
  ProgramUniform3dv := @glCapProgramUniform3dv;
  ProgramUniform3ui := @glCapProgramUniform3ui;
  ProgramUniform3uiv := @glCapProgramUniform3uiv;
  ProgramUniform4i := @glCapProgramUniform4i;
  ProgramUniform4iv := @glCapProgramUniform4iv;
  ProgramUniform4f := @glCapProgramUniform4f;
  ProgramUniform4fv := @glCapProgramUniform4fv;
  ProgramUniform4d := @glCapProgramUniform4d;
  ProgramUniform4dv := @glCapProgramUniform4dv;
  ProgramUniform4ui := @glCapProgramUniform4ui;
  ProgramUniform4uiv := @glCapProgramUniform4uiv;
  ProgramUniformMatrix2fv := @glCapProgramUniformMatrix2fv;
  ProgramUniformMatrix3fv := @glCapProgramUniformMatrix3fv;
  ProgramUniformMatrix4fv := @glCapProgramUniformMatrix4fv;
  ProgramUniformMatrix2dv := @glCapProgramUniformMatrix2dv;
  ProgramUniformMatrix3dv := @glCapProgramUniformMatrix3dv;
  ProgramUniformMatrix4dv := @glCapProgramUniformMatrix4dv;
  ProgramUniformMatrix2x3fv := @glCapProgramUniformMatrix2x3fv;
  ProgramUniformMatrix3x2fv := @glCapProgramUniformMatrix3x2fv;
  ProgramUniformMatrix2x4fv := @glCapProgramUniformMatrix2x4fv;
  ProgramUniformMatrix4x2fv := @glCapProgramUniformMatrix4x2fv;
  ProgramUniformMatrix3x4fv := @glCapProgramUniformMatrix3x4fv;
  ProgramUniformMatrix4x3fv := @glCapProgramUniformMatrix4x3fv;
  ProgramUniformMatrix2x3dv := @glCapProgramUniformMatrix2x3dv;
  ProgramUniformMatrix3x2dv := @glCapProgramUniformMatrix3x2dv;
  ProgramUniformMatrix2x4dv := @glCapProgramUniformMatrix2x4dv;
  ProgramUniformMatrix4x2dv := @glCapProgramUniformMatrix4x2dv;
  ProgramUniformMatrix3x4dv := @glCapProgramUniformMatrix3x4dv;
  ProgramUniformMatrix4x3dv := @glCapProgramUniformMatrix4x3dv;
  ValidateProgramPipeline := @glCapValidateProgramPipeline;
  GetProgramPipelineInfoLog := @glCapGetProgramPipelineInfoLog;

  DrawBuffers := @glCapDrawBuffers;

  ActiveTexture := @glCapActiveTexture;
  CompressedTexImage3D := @glCapCompressedTexImage3D;
  CompressedTexImage2D := @glCapCompressedTexImage2D;
  CompressedTexImage1D := @glCapCompressedTexImage1D;
  CompressedTexSubImage3D := @glCapCompressedTexSubImage3D;
  CompressedTexSubImage2D := @glCapCompressedTexSubImage2D;
  CompressedTexSubImage1D := @glCapCompressedTexSubImage1D;
  GetCompressedTexImage := @glCapGetCompressedTexImage;
  ClientActiveTexture := @glCapClientActiveTexture;
  MultiTexCoord1d := @glCapMultiTexCoord1d;
  MultiTexCoord1dV := @glCapMultiTexCoord1dV;
  MultiTexCoord1f := @glCapMultiTexCoord1f;
  MultiTexCoord1fv := @glCapMultiTexCoord1fv;
  MultiTexCoord1i := @glCapMultiTexCoord1i;
  MultiTexCoord1iv := @glCapMultiTexCoord1iv;
  MultiTexCoord1s := @glCapMultiTexCoord1s;
  MultiTexCoord1sv := @glCapMultiTexCoord1sv;
  MultiTexCoord2d := @glCapMultiTexCoord2d;
  MultiTexCoord2dv := @glCapMultiTexCoord2dv;
  MultiTexCoord2f := @glCapMultiTexCoord2f;
  MultiTexCoord2fv := @glCapMultiTexCoord2fv;
  MultiTexCoord2i := @glCapMultiTexCoord2i;
  MultiTexCoord2iv := @glCapMultiTexCoord2iv;
  MultiTexCoord2s := @glCapMultiTexCoord2s;
  MultiTexCoord2sv := @glCapMultiTexCoord2sv;
  MultiTexCoord3d := @glCapMultiTexCoord3d;
  MultiTexCoord3dv := @glCapMultiTexCoord3dv;
  MultiTexCoord3f := @glCapMultiTexCoord3f;
  MultiTexCoord3fv := @glCapMultiTexCoord3fv;
  MultiTexCoord3i := @glCapMultiTexCoord3i;
  MultiTexCoord3iv := @glCapMultiTexCoord3iv;
  MultiTexCoord3s := @glCapMultiTexCoord3s;
  MultiTexCoord3sv := @glCapMultiTexCoord3sv;
  MultiTexCoord4d := @glCapMultiTexCoord4d;
  MultiTexCoord4dv := @glCapMultiTexCoord4dv;
  MultiTexCoord4f := @glCapMultiTexCoord4f;
  MultiTexCoord4fv := @glCapMultiTexCoord4fv;
  MultiTexCoord4i := @glCapMultiTexCoord4i;
  MultiTexCoord4iv := @glCapMultiTexCoord4iv;
  MultiTexCoord4s := @glCapMultiTexCoord4s;
  MultiTexCoord4sv := @glCapMultiTexCoord4sv;

  GetInteger64i_v := @glCapGetInteger64i_v;
  GetBufferParameteri64v := @glCapGetBufferParameteri64v;
  ProgramParameteri := @glCapProgramParameteri;

  ProgramString := @glCapProgramString;
  BindProgram := @glCapBindProgram;
  DeletePrograms := @glCapDeletePrograms;
  GenPrograms := @glCapGenPrograms;
  ProgramEnvParameter4d := @glCapProgramEnvParameter4d;
  ProgramEnvParameter4dv := @glCapProgramEnvParameter4dv;
  ProgramEnvParameter4f := @glCapProgramEnvParameter4f;
  ProgramEnvParameter4fv := @glCapProgramEnvParameter4fv;
  ProgramLocalParameter4d := @glCapProgramLocalParameter4d;
  ProgramLocalParameter4dv := @glCapProgramLocalParameter4dv;
  ProgramLocalParameter4f := @glCapProgramLocalParameter4f;
  ProgramLocalParameter4fv := @glCapProgramLocalParameter4fv;
  GetProgramEnvParameterdv := @glCapGetProgramEnvParameterdv;
  GetProgramEnvParameterfv := @glCapGetProgramEnvParameterfv;
  GetProgramLocalParameterdv := @glCapGetProgramLocalParameterdv;
  GetProgramLocalParameterfv := @glCapGetProgramLocalParameterfv;

  ClearColorIi := @glCapClearColorIi;
  ClearColorIui := @glCapClearColorIui;
  TexParameterIiv := @glCapTexParameterIiv;
  TexParameterIuiv := @glCapTexParameterIuiv;
  GetTexParameterIiv := @glCapGetTexParameterIiv;
  GetTexParameterIuiv := @glCapGetTexParameterIuiv;
  PatchParameteri := @glCapPatchParameteri;
  PatchParameterfv := @glCapPatchParameterfv;

  BufferAddressRangeNV := @glCapBufferAddressRangeNV;
  VertexFormatNV := @glCapVertexFormatNV;
  NormalFormatNV := @glCapNormalFormatNV;
  ColorFormatNV := @glCapColorFormatNV;
  IndexFormatNV := @glCapIndexFormatNV;
  TexCoordFormatNV := @glCapTexCoordFormatNV;
  EdgeFlagFormatNV := @glCapEdgeFlagFormatNV;
  SecondaryColorFormatNV := @glCapSecondaryColorFormatNV;
  FogCoordFormatNV := @glCapFogCoordFormatNV;
  VertexAttribFormatNV := @glCapVertexAttribFormatNV;
  VertexAttribIFormatNV := @glCapVertexAttribIFormatNV;
  GetIntegerui64i_vNV := @glCapGetIntegerui64i_vNV;
  GetBufferParameterui64vNV := @glCapGetBufferParameterui64vNV;
  MakeBufferResidentNV := @glCapMakeBufferResidentNV;
  MakeBufferNonResidentNV := @glCapMakeBufferNonResidentNV;
  IsBufferResidentNV := @glCapIsBufferResidentNV;
  MakeNamedBufferResidentNV := @glCapMakeNamedBufferResidentNV;
  MakeNamedBufferNonResidentNV := @glCapMakeNamedBufferNonResidentNV;
  IsNamedBufferResidentNV := @glCapIsNamedBufferResidentNV;
  GetNamedBufferParameterui64vNV := @glCapGetNamedBufferParameterui64vNV;
  GetIntegerui64vNV := @glCapGetIntegerui64vNV;
  Uniformui64NV := @glCapUniformui64NV;
  Uniformui64vNV := @glCapUniformui64vNV;
  GetUniformui64vNV := @glCapGetUniformui64vNV;
  ProgramUniformui64NV := @glCapProgramUniformui64NV;
  ProgramUniformui64vNV := @glCapProgramUniformui64vNV;

  TexImage2DMultisample := @glCapTexImage2DMultisample;
  TexImage3DMultisample := @glCapTexImage3DMultisample;
  GetMultisamplefv := @glCapGetMultisamplefv;
  SampleMaski := @glCapSampleMaski;

  ProvokingVertex := @glCapProvokingVertex;

  FenceSync := @glCapFenceSync;
  IsSync := @glCapIsSync;
  DeleteSync := @glCapDeleteSync;
  ClientWaitSync := @glCapClientWaitSync;
  WaitSync := @glCapWaitSync;
  GetInteger64v := @glCapGetInteger64v;
  GetSynciv := @glCapGetSynciv;

  BlendEquationi := @glCapBlendEquationi;
  BlendEquationSeparatei := @glCapBlendEquationSeparatei;
  BlendFunci := @glCapBlendFunci;
  BlendFuncSeparatei := @glCapBlendFuncSeparatei;
  MinSampleShading := @glCapMinSampleShading;

  GenSamplers := @glCapGenSamplers;
  DeleteSamplers := @glCapDeleteSamplers;
  IsSampler := @glCapIsSampler;
  BindSampler := @glCapBindSampler;
  SamplerParameteri := @glCapSamplerParameteri;
  SamplerParameteriv := @glCapSamplerParameteriv;
  SamplerParameterf := @glCapSamplerParameterf;
  SamplerParameterfv := @glCapSamplerParameterfv;
  SamplerParameterIiv := @glCapSamplerParameterIiv;
  SamplerParameterIuiv := @glCapSamplerParameterIuiv;
  GetSamplerParameteriv := @glCapGetSamplerParameteriv;
  GetSamplerParameterIiv := @glCapGetSamplerParameterIiv;
  GetSamplerParameterfv := @glCapGetSamplerParameterfv;
  GetSamplerParameterIfv := @glCapGetSamplerParameterIfv;

  ClientAttribDefault := @glCapClientAttribDefault;
  PushClientAttribDefault := @glCapPushClientAttribDefault;
  MatrixLoadf := @glCapMatrixLoadf;
  MatrixLoadd := @glCapMatrixLoadd;
  MatrixMultf := @glCapMatrixMultf;
  MatrixMultd := @glCapMatrixMultd;
  MatrixLoadIdentity := @glCapMatrixLoadIdentity;
  MatrixRotatef := @glCapMatrixRotatef;
  MatrixRotated := @glCapMatrixRotated;
  MatrixScalef := @glCapMatrixScalef;
  MatrixScaled := @glCapMatrixScaled;
  MatrixTranslatef := @glCapMatrixTranslatef;
  MatrixTranslated := @glCapMatrixTranslated;
  MatrixFrustum := @glCapMatrixFrustum;
  MatrixOrtho := @glCapMatrixOrtho;
  MatrixPop := @glCapMatrixPop;
  MatrixPush := @glCapMatrixPush;
  MatrixLoadTransposef := @glCapMatrixLoadTransposef;
  MatrixLoadTransposed := @glCapMatrixLoadTransposed;
  MatrixMultTransposef := @glCapMatrixMultTransposef;
  MatrixMultTransposed := @glCapMatrixMultTransposed;
  TextureParameterf := @glCapTextureParameterf;
  TextureParameterfv := @glCapTextureParameterfv;
  TextureParameteri := @glCapTextureParameteri;
  TextureParameteriv := @glCapTextureParameteriv;
  TextureImage1D := @glCapTextureImage1D;
  TextureImage2D := @glCapTextureImage2D;
  TextureSubImage1D := @glCapTextureSubImage1D;
  TextureSubImage2D := @glCapTextureSubImage2D;
  CopyTextureImage1D := @glCapCopyTextureImage1D;
  CopyTextureImage2D := @glCapCopyTextureImage2D;
  CopyTextureSubImage1D := @glCapCopyTextureSubImage1D;
  CopyTextureSubImage2D := @glCapCopyTextureSubImage2D;
  GetTextureImage := @glCapGetTextureImage;
  GetTextureParameterfv := @glCapGetTextureParameterfv;
  GetTextureParameteriv := @glCapGetTextureParameteriv;
  GetTextureLevelParameterfv := @glCapGetTextureLevelParameterfv;
  GetTextureLevelParameteriv := @glCapGetTextureLevelParameteriv;
  TextureImage3D := @glCapTextureImage3D;
  TextureSubImage3D := @glCapTextureSubImage3D;
  CopyTextureSubImage3D := @glCapCopyTextureSubImage3D;
  MultiTexParameterf := @glCapMultiTexParameterf;
  MultiTexParameterfv := @glCapMultiTexParameterfv;
  MultiTexParameteri := @glCapMultiTexParameteri;
  MultiTexParameteriv := @glCapMultiTexParameteriv;
  MultiTexImage1D := @glCapMultiTexImage1D;
  MultiTexImage2D := @glCapMultiTexImage2D;
  MultiTexSubImage1D := @glCapMultiTexSubImage1D;
  MultiTexSubImage2D := @glCapMultiTexSubImage2D;
  CopyMultiTexImage1D := @glCapCopyMultiTexImage1D;
  CopyMultiTexImage2D := @glCapCopyMultiTexImage2D;
  CopyMultiTexSubImage1D := @glCapCopyMultiTexSubImage1D;
  CopyMultiTexSubImage2D := @glCapCopyMultiTexSubImage2D;
  GetMultiTexImage := @glCapGetMultiTexImage;
  GetMultiTexParameterfv := @glCapGetMultiTexParameterfv;
  GetMultiTexParameteriv := @glCapGetMultiTexParameteriv;
  GetMultiTexLevelParameterfv := @glCapGetMultiTexLevelParameterfv;
  GetMultiTexLevelParameteriv := @glCapGetMultiTexLevelParameteriv;
  MultiTexImage3D := @glCapMultiTexImage3D;
  MultiTexSubImage3D := @glCapMultiTexSubImage3D;
  CopyMultiTexSubImage3D := @glCapCopyMultiTexSubImage3D;
  BindMultiTexture := @glCapBindMultiTexture;
  EnableClientStateIndexed := @glCapEnableClientStateIndexed;
  DisableClientStateIndexed := @glCapDisableClientStateIndexed;
  MultiTexCoordPointer := @glCapMultiTexCoordPointer;
  MultiTexEnvf := @glCapMultiTexEnvf;
  MultiTexEnvfv := @glCapMultiTexEnvfv;
  MultiTexEnvi := @glCapMultiTexEnvi;
  MultiTexEnviv := @glCapMultiTexEnviv;
  MultiTexGend := @glCapMultiTexGend;
  MultiTexGendv := @glCapMultiTexGendv;
  MultiTexGenf := @glCapMultiTexGenf;
  MultiTexGenfv := @glCapMultiTexGenfv;
  MultiTexGeni := @glCapMultiTexGeni;
  MultiTexGeniv := @glCapMultiTexGeniv;
  GetMultiTexEnvfv := @glCapGetMultiTexEnvfv;
  GetMultiTexEnviv := @glCapGetMultiTexEnviv;
  GetMultiTexGendv := @glCapGetMultiTexGendv;
  GetMultiTexGenfv := @glCapGetMultiTexGenfv;
  GetMultiTexGeniv := @glCapGetMultiTexGeniv;
  GetFloatIndexedv := @glCapGetFloatIndexedv;
  GetDoubleIndexedv := @glCapGetDoubleIndexedv;
  GetPointerIndexedv := @glCapGetPointerIndexedv;
  CompressedTextureImage3D := @glCapCompressedTextureImage3D;
  CompressedTextureImage2D := @glCapCompressedTextureImage2D;
  CompressedTextureImage1D := @glCapCompressedTextureImage1D;
  CompressedTextureSubImage3D := @glCapCompressedTextureSubImage3D;
  CompressedTextureSubImage2D := @glCapCompressedTextureSubImage2D;
  CompressedTextureSubImage1D := @glCapCompressedTextureSubImage1D;
  GetCompressedTextureImage := @glCapGetCompressedTextureImage;
  CompressedMultiTexImage3D := @glCapCompressedMultiTexImage3D;
  CompressedMultiTexImage2D := @glCapCompressedMultiTexImage2D;
  CompressedMultiTexImage1D := @glCapCompressedMultiTexImage1D;
  CompressedMultiTexSubImage3D := @glCapCompressedMultiTexSubImage3D;
  CompressedMultiTexSubImage2D := @glCapCompressedMultiTexSubImage2D;
  CompressedMultiTexSubImage1D := @glCapCompressedMultiTexSubImage1D;
  GetCompressedMultiTexImage := @glCapGetCompressedMultiTexImage;
  NamedProgramString := @glCapNamedProgramString;
  NamedProgramLocalParameter4d := @glCapNamedProgramLocalParameter4d;
  NamedProgramLocalParameter4dv := @glCapNamedProgramLocalParameter4dv;
  NamedProgramLocalParameter4f := @glCapNamedProgramLocalParameter4f;
  NamedProgramLocalParameter4fv := @glCapNamedProgramLocalParameter4fv;
  GetNamedProgramLocalParameterdv := @glCapGetNamedProgramLocalParameterdv;
  GetNamedProgramLocalParameterfv := @glCapGetNamedProgramLocalParameterfv;
  GetNamedProgramiv := @glCapGetNamedProgramiv;
  GetNamedProgramString := @glCapGetNamedProgramString;
  NamedProgramLocalParameters4fv := @glCapNamedProgramLocalParameters4fv;
  NamedProgramLocalParameterI4i := @glCapNamedProgramLocalParameterI4i;
  NamedProgramLocalParameterI4iv := @glCapNamedProgramLocalParameterI4iv;
  NamedProgramLocalParametersI4iv := @glCapNamedProgramLocalParametersI4iv;
  NamedProgramLocalParameterI4ui := @glCapNamedProgramLocalParameterI4ui;
  NamedProgramLocalParameterI4uiv := @glCapNamedProgramLocalParameterI4uiv;
  NamedProgramLocalParametersI4uiv := @glCapNamedProgramLocalParametersI4uiv;
  GetNamedProgramLocalParameterIiv := @glCapGetNamedProgramLocalParameterIiv;
  GetNamedProgramLocalParameterIuiv := @glCapGetNamedProgramLocalParameterIuiv;
  TextureParameterIiv := @glCapTextureParameterIiv;
  TextureParameterIuiv := @glCapTextureParameterIuiv;
  GetTextureParameterIiv := @glCapGetTextureParameterIiv;
  GetTextureParameterIuiv := @glCapGetTextureParameterIuiv;
  MultiTexParameterIiv := @glCapMultiTexParameterIiv;
  MultiTexParameterIuiv := @glCapMultiTexParameterIuiv;
  GetMultiTexParameterIiv := @glCapGetMultiTexParameterIiv;
  GetMultiTexParameterIuiv := @glCapGetMultiTexParameterIuiv;
  NamedBufferData := @glCapNamedBufferData;
  NamedBufferSubData := @glCapNamedBufferSubData;
  MapNamedBuffer := @glCapMapNamedBuffer;
  UnmapNamedBuffer := @glCapUnmapNamedBuffer;
  MapNamedBufferRange := @glCapMapNamedBufferRange;
  FlushMappedNamedBufferRange := @glCapFlushMappedNamedBufferRange;
  NamedCopyBufferSubData := @glCapNamedCopyBufferSubData;
  GetNamedBufferParameteriv := @glCapGetNamedBufferParameteriv;
  GetNamedBufferPointerv := @glCapGetNamedBufferPointerv;
  GetNamedBufferSubData := @glCapGetNamedBufferSubData;
  TextureBuffer := @glCapTextureBuffer;
  MultiTexBuffer := @glCapMultiTexBuffer;
  NamedRenderbufferStorage := @glCapNamedRenderbufferStorage;
  GetNamedRenderbufferParameteriv := @glCapGetNamedRenderbufferParameteriv;
  CheckNamedFramebufferStatus := @glCapCheckNamedFramebufferStatus;
  NamedFramebufferTexture1D := @glCapNamedFramebufferTexture1D;
  NamedFramebufferTexture2D := @glCapNamedFramebufferTexture2D;
  NamedFramebufferTexture3D := @glCapNamedFramebufferTexture3D;
  NamedFramebufferRenderbuffer := @glCapNamedFramebufferRenderbuffer;
  GetNamedFramebufferAttachmentParameteriv := @glCapGetNamedFramebufferAttachmentParameteriv;
  GenerateTextureMipmap := @glCapGenerateTextureMipmap;
  GenerateMultiTexMipmap := @glCapGenerateMultiTexMipmap;
  FramebufferDrawBuffer := @glCapFramebufferDrawBuffer;
  FramebufferDrawBuffers := @glCapFramebufferDrawBuffers;
  FramebufferReadBuffer := @glCapFramebufferReadBuffer;
  GetFramebufferParameteriv := @glCapGetFramebufferParameteriv;
  NamedRenderbufferStorageMultisample := @glCapNamedRenderbufferStorageMultisample;
  NamedRenderbufferStorageMultisampleCoverage := @glCapNamedRenderbufferStorageMultisampleCoverage;
  NamedFramebufferTexture := @glCapNamedFramebufferTexture;
  NamedFramebufferTextureLayer := @glCapNamedFramebufferTextureLayer;
  NamedFramebufferTextureFace := @glCapNamedFramebufferTextureFace;
  TextureRenderbuffer := @glCapTextureRenderbuffer;
  MultiTexRenderbuffer := @glCapMultiTexRenderbuffer;

  FrameTerminatorGREMEDY := @glCapFrameTerminatorGREMEDY;
  StringMarkerGREMEDY := @glCapStringMarkerGREMEDY;
  DebugMessageControl := @glCapDebugMessageControl;
  DebugMessageInsert := @glCapDebugMessageInsert;
  DebugMessageCallback := @glCapDebugMessageCallback;
  GetDebugMessageLog := @glCapGetDebugMessageLog;

  GenPathsNV := @glCapGenPathsNV;
  DeletePathsNV := @glCapDeletePathsNV;
  IsPathNV := @glCapIsPathNV;
  PathCommandsNV := @glCapPathCommandsNV;
  PathCoordsNV := @glCapPathCoordsNV;
  PathSubCommandsNV := @glCapPathSubCommandsNV;
  PathSubCoordsNV := @glCapPathSubCoordsNV;
  PathStringNV := @glCapPathStringNV;
  PathGlyphsNV := @glCapPathGlyphsNV;
  PathGlyphRangeNV := @glCapPathGlyphRangeNV;
  WeightPathsNV := @glCapWeightPathsNV;
  CopyPathNV := @glCapCopyPathNV;
  InterpolatePathsNV := @glCapInterpolatePathsNV;
  PathParameterivNV := @glCapPathParameterivNV;
  PathParameteriNV := @glCapPathParameteriNV;
  PathParameterfvNV := @glCapPathParameterfvNV;
  PathParameterfNV := @glCapPathParameterfNV;
  PathDashArrayNV := @glCapPathDashArrayNV;
  PathStencilFuncNV := @glCapPathStencilFuncNV;
  StencilFillPathNV := @glCapStencilFillPathNV;
  StencilStrokePathNV := @glCapStencilStrokePathNV;
  StencilFillPathInstancedNV := @glCapStencilFillPathInstancedNV;
  StencilStrokePathInstancedNV := @glCapStencilStrokePathInstancedNV;
  PathColorGenNV := @glCapPathColorGenNV;
  PathTexGenNV := @glCapPathTexGenNV;
  PathFogGenNV := @glCapPathFogGenNV;
  CoverFillPathNV := @glCapCoverFillPathNV;
  CoverStrokePathNV := @glCapCoverStrokePathNV;
  CoverFillPathInstancedNV := @glCapCoverFillPathInstancedNV;
  CoverStrokePathInstancedNV := @glCapCoverStrokePathInstancedNV;
  GetPathParameterivNV := @glCapGetPathParameterivNV;
  GetPathParameterfvNV := @glCapGetPathParameterfvNV;
  GetPathCommandsNV := @glCapGetPathCommandsNV;
  GetPathCoordsNV := @glCapGetPathCoordsNV;
  GetPathDashArrayNV := @glCapGetPathDashArrayNV;
  GetPathMetricsNV := @glCapGetPathMetricsNV;
  GetPathMetricRangeNV := @glCapGetPathMetricRangeNV;
  GetPathSpacingNV := @glCapGetPathSpacingNV;
  GetPathColorGenivNV := @glCapGetPathColorGenivNV;
  GetPathColorGenfvNV := @glCapGetPathColorGenfvNV;
  GetPathTexGenivNV := @glCapGetPathTexGenivNV;
  GetPathTexGenfvNV := @glCapGetPathTexGenfvNV;
  IsPointInFillPathNV := @glCapIsPointInFillPathNV;
  IsPointInStrokePathNV := @glCapIsPointInStrokePathNV;
  GetPathLengthNV := @glCapGetPathLengthNV;
  PointAlongPathNV := @glCapPointAlongPathNV;
  PathStencilDepthOffsetNV := @glCapPathStencilDepthOffsetNV;
  PathCoverDepthFuncNV := @glCapPathCoverDepthFuncNV;

  FInitialized := False;
end;

{$IFDEF SUPPORT_WGL}
// ReadWGLImplementationProperties


procedure TGLExtensionsAndEntryPoints.ReadWGLImplementationProperties;
begin
  // ARB wgl extensions
  if @WGetExtensionsStringARB <> @glCapWGetExtensionsStringARB then
  begin
    FBuffer := string(WGetExtensionsStringARB(wglGetCurrentDC));
    W_ARB_buffer_region := CheckExtension('WGL_ARB_buffer_region');
    W_ARB_create_context := CheckExtension('WGL_ARB_create_context');
    W_ARB_create_context_profile := CheckExtension('WGL_ARB_create_context_profile');
    W_ARB_extensions_string := CheckExtension('WGL_ARB_extensions_string');
    W_ARB_framebuffer_sRGB := CheckExtension('WGL_ARB_framebuffer_sRGB');
    W_ARB_make_current_read := CheckExtension('WGL_ARB_make_current_read');
    W_ARB_multisample := CheckExtension('WGL_ARB_multisample');
    W_ARB_pbuffer := CheckExtension('WGL_ARB_pbuffer');
    W_ARB_pixel_format := CheckExtension('WGL_ARB_pixel_format');
    W_ARB_pixel_format_float := CheckExtension('WGL_ARB_pixel_format_float');
    W_ARB_render_texture := CheckExtension('WGL_ARB_render_texture');
    // Vendor/EXT wgl extensions
    W_ATI_pixel_format_float := CheckExtension('WGL_ATI_pixel_format_float');
    W_EXT_framebuffer_sRGB := CheckExtension('WGL_EXT_framebuffer_sRGB');
    W_EXT_pixel_format_packed_float := CheckExtension('WGL_EXT_pixel_format_packed_float');
    W_EXT_swap_control := CheckExtension('WGL_EXT_swap_control');
    W_NV_gpu_affinity := CheckExtension('WGL_NV_gpu_affinity');
    W_EXT_create_context_es2_profile := CheckExtension('WGL_EXT_create_context_es2_profile');
    W_NV_DX_interop := CheckExtension('WGL_NV_DX_interop');
  end;
end;

// ReadWGLExtensions


procedure TGLExtensionsAndEntryPoints.ReadWGLExtensions;
begin
  // ARB wgl extensions

  // ###########################################################
  // locating functions and procedures for
  // ARB approved WGL extensions
  // ###########################################################

  // WGL_buffer_region (ARB #4)
  WCreateBufferRegionARB := GLGetProcAddress('wglCreateBufferRegionARB',@glCapWCreateBufferRegionARB);
  WDeleteBufferRegionARB := GLGetProcAddress('wglDeleteBufferRegionARB',@glCapWDeleteBufferRegionARB);
  WSaveBufferRegionARB := GLGetProcAddress('wglSaveBufferRegionARB',@glCapWSaveBufferRegionARB);
  WRestoreBufferRegionARB := GLGetProcAddress('wglRestoreBufferRegionARB',@glCapWRestoreBufferRegionARB);

  // WGL_ARB_extensions_string (ARB #8)
  WGetExtensionsStringARB := GLGetProcAddress('wglGetExtensionsStringARB',@glCapWGetExtensionsStringARB);

  // WGL_ARB_pixel_format (ARB #9)
  WGetPixelFormatAttribivARB := GLGetProcAddress('wglGetPixelFormatAttribivARB',@glCapWGetPixelFormatAttribivARB);
  WGetPixelFormatAttribfvARB := GLGetProcAddress('wglGetPixelFormatAttribfvARB',@glCapWGetPixelFormatAttribfvARB);
  WChoosePixelFormatARB := GLGetProcAddress('wglChoosePixelFormatARB',@glCapWChoosePixelFormatARB);

  // WGL_make_current_read (ARB #10)
  WMakeContextCurrentARB := GLGetProcAddress('wglMakeContextCurrentARB',@glCapWMakeContextCurrentARB);
  WGetCurrentReadDCARB := GLGetProcAddress('wglGetCurrentReadDCARB',@glCapWGetCurrentReadDCARB);

  // WGL_ARB_pbuffer (ARB #11)
  WCreatePbufferARB := GLGetProcAddress('wglCreatePbufferARB',@glCapWCreatePbufferARB);
  WGetPbufferDCARB := GLGetProcAddress('wglGetPbufferDCARB',@glCapWGetPbufferDCARB);
  WReleasePbufferDCARB := GLGetProcAddress('wglReleasePbufferDCARB',@glCapWReleasePbufferDCARB);
  WDestroyPbufferARB := GLGetProcAddress('wglDestroyPbufferARB',@glCapWDestroyPbufferARB);
  WQueryPbufferARB := GLGetProcAddress('wglQueryPbufferARB',@glCapWQueryPbufferARB);

  // WGL_ARB_render_texture (ARB #20)
  WBindTexImageARB := GLGetProcAddress('wglBindTexImageARB',@glCapWBindTexImageARB);
  WReleaseTexImageARB := GLGetProcAddress('wglReleaseTexImageARB',@glCapWReleaseTexImageARB);
  WSetPbufferAttribARB := GLGetProcAddress('wglSetPbufferAttribARB',@glCapWSetPbufferAttribARB);

  // WGL_ARB_create_context (ARB #55)
  WCreateContextAttribsARB := GLGetProcAddress('wglCreateContextAttribsARB',@glCapWCreateContextAttribsARB);

  // ###########################################################
  // locating functions and procedures for
  // Vendor/EXT WGL extensions
  // ###########################################################

  // WGL_EXT_swap_control (EXT #172)
  WSwapIntervalEXT := GLGetProcAddress('wglSwapIntervalEXT',@glCapWSwapIntervalEXT);
  WGetSwapIntervalEXT := GLGetProcAddress('wglGetSwapIntervalEXT',@glCapWGetSwapIntervalEXT);

  // GL_NV_vertex_array_range (EXT #190)
  WAllocateMemoryNV := GLGetProcAddress('wglAllocateMemoryNV',@glCapWAllocateMemoryNV);
  WFreeMemoryNV := GLGetProcAddress('wglFreeMemoryNV',@glCapWFreeMemoryNV);

  // WGL_NV_gpu_affinity
  WEnumGpusNV := GLGetProcAddress('wglEnumGpusNV',@glCapWEnumGpusNV);
  WEnumGpuDevicesNV := GLGetProcAddress('wglEnumGpuDevicesNV',@glCapWEnumGpuDevicesNV);
  WCreateAffinityDCNV := GLGetProcAddress('wglCreateAffinityDCNV',@glCapWCreateAffinityDCNV);
  WEnumGpusFromAffinityDCNV := GLGetProcAddress('wglEnumGpusFromAffinityDCNV',@glCapWEnumGpusFromAffinityDCNV);
  WDeleteDCNV := GLGetProcAddress('wglDeleteDCNV',@glCapWDeleteDCNV);

  // WGL_NV_DX_interop
  WDXSetResourceShareHandleNV := GLGetProcAddress('wglDXSetResourceShareHandleNV',@glCapWDXSetResourceShareHandleNV);
  WDXOpenDeviceNV := GLGetProcAddress('wglDXOpenDeviceNV',@glCapWDXOpenDeviceNV);
  WDXCloseDeviceNV := GLGetProcAddress('wglDXCloseDeviceNV',@glCapWDXCloseDeviceNV);
  WDXRegisterObjectNV := GLGetProcAddress('wglDXRegisterObjectNV',@glCapWDXRegisterObjectNV);
  WDXUnregisterObjectNV := GLGetProcAddress('wglDXUnregisterObjectNV',@glCapWDXUnregisterObjectNV);
  WDXObjectAccessNV := GLGetProcAddress('wglDXObjectAccessNV',@glCapWDXObjectAccessNV);
  WDXLockObjectsNV := GLGetProcAddress('wglDXLockObjectsNV',@glCapWDXLockObjectsNV);
  WDXUnlockObjectsNV := GLGetProcAddress('wglDXUnlockObjectsNV',@glCapWDXUnlockObjectsNV);
end;

{$ENDIF}
{$IFDEF SUPPORT_GLX}
// ReadGLXImplementationProperties


procedure TGLExtensionsAndEntryPoints.ReadGLXImplementationProperties;
var
  MajorVersion, MinorVersion: integer;
  dpy: PDisplay;
begin
  dpy := glXGetCurrentDisplay();
  FBuffer := string(glXQueryServerString(dpy, XDefaultScreen(dpy), GLX_VERSION));
  TrimAndSplitVersionString(FBuffer, MajorVersion, MinorVersion);
  X_VERSION_1_1 := IsVersionMet(1, 1, MajorVersion, MinorVersion);
  X_VERSION_1_2 := IsVersionMet(1, 2, MajorVersion, MinorVersion);
  X_VERSION_1_3 := IsVersionMet(1, 3, MajorVersion, MinorVersion);
  X_VERSION_1_4 := IsVersionMet(1, 4, MajorVersion, MinorVersion);

  // This procedure will probably need changing, as totally untested
  // This might only work if GLX functions/procedures are loaded dynamically
  if Assigned(glXQueryExtensionsString) then
    FBuffer := glXQueryExtensionsString(dpy, 0) // guess at a valid screen
  else
    FBuffer := '';
  X_ARB_create_context := CheckExtension('GLX_ARB_create_context');
  X_ARB_create_context_profile := CheckExtension('GLX_ARB_create_context_profile');
  X_ARB_framebuffer_sRGB := CheckExtension('GLX_ARB_framebuffer_sRGB');
  X_EXT_framebuffer_sRGB := CheckExtension('GLX_EXT_framebuffer_sRGB');
  X_EXT_fbconfig_packed_float := CheckExtension('GLX_EXT_fbconfig_packed_float');
  X_SGI_swap_control := CheckExtension('GLX_SGI_swap_control');
  X_ARB_multisample := CheckExtension('GLX_ARB_multisample');

  X_SGIS_multisample := CheckExtension('GLX_SGIS_multisample');
  X_EXT_visual_info := CheckExtension('GLX_EXT_visual_info');
  X_SGI_video_sync := CheckExtension('GLX_SGI_video_sync');
  X_SGI_make_current_read := CheckExtension('GLX_SGI_make_current_read');
  X_SGIX_video_source := CheckExtension('GLX_SGIX_video_source');
  X_EXT_visual_rating := CheckExtension('GLX_EXT_visual_rating');
  X_EXT_import_context := CheckExtension('GLX_EXT_import_context');
  X_SGIX_fbconfig := CheckExtension('GLX_SGIX_fbconfig');
  X_SGIX_pbuffer := CheckExtension('GLX_SGIX_pbuffer');
  X_SGI_cushion := CheckExtension('GLX_SGI_cushion');
  X_SGIX_video_resize := CheckExtension('GLX_SGIX_video_resize');
  X_SGIX_dmbuffer := CheckExtension('GLX_SGIX_dmbuffer');
  X_SGIX_swap_group := CheckExtension('GLX_SGIX_swap_group');
  X_SGIX_swap_barrier := CheckExtension('GLX_SGIX_swap_barrier');
  X_SGIS_blended_overlay := CheckExtension('GLX_SGIS_blended_overlay');
  X_SGIS_shared_multisample := CheckExtension('GLX_SGIS_shared_multisample');
  X_SUN_get_transparent_index := CheckExtension('GLX_SUN_get_transparent_index');
  X_3DFX_multisample := CheckExtension('GLX_3DFX_multisample');
  X_MESA_copy_sub_buffer := CheckExtension('GLX_MESA_copy_sub_buffer');
  X_MESA_pixmap_colormap := CheckExtension('GLX_MESA_pixmap_colormap');
  X_MESA_release_buffers := CheckExtension('GLX_MESA_release_buffers');
  X_MESA_set_3dfx_mode := CheckExtension('GLX_MESA_set_3dfx_mode');
  X_SGIX_visual_select_group := CheckExtension('GLX_SGIX_visual_select_group');
  X_SGIX_hyperpipe := CheckExtension('GLX_SGIX_hyperpipe');
  X_NV_multisample_coverage := CheckExtension('GLX_NV_multisample_coverage');
end;

// ReadGLXExtensions


procedure TGLExtensionsAndEntryPoints.ReadGLXExtensions;
begin
  // ARB glx extensions

  // ###########################################################
  // locating functions and procedures for
  // ARB approved GLX extensions
  // ###########################################################

  // GLX 1.3 and later
  XChooseFBConfig := GLGetProcAddress('glXChooseFBConfig',@glCapXChooseFBConfig);
  XGetFBConfigAttrib := GLGetProcAddress('glXGetFBConfigAttrib',@glCapXGetFBConfigAttrib);
  XGetFBConfigs := GLGetProcAddress('glXGetFBConfigs',@glCapXGetFBConfigs);
  XGetVisualFromFBConfig := GLGetProcAddress('glXGetVisualFromFBConfig',@glCapXGetVisualFromFBConfig);
  XCreateWindow := GLGetProcAddress('glXCreateWindow',@glCapXCreateWindow);
  XDestroyWindow := GLGetProcAddress('glXDestroyWindow',@glCapXDestroyWindow);
  XCreatePixmap := GLGetProcAddress('glXCreatePixmap',@glCapXCreatePixmap);
  XDestroyPixmap := GLGetProcAddress('glXDestroyPixmap',@glCapXDestroyPixmap);
  XCreatePbuffer := GLGetProcAddress('glXCreatePbuffer',@glCapXCreatePbuffer);
  XDestroyPbuffer := GLGetProcAddress('glXDestroyPbuffer',@glCapXDestroyPbuffer);
  XQueryDrawable := GLGetProcAddress('glXQueryDrawable',@glCapXQueryDrawable);
  XCreateNewContext := GLGetProcAddress('glXCreateNewContext',@glCapXCreateNewContext);
  XMakeContextCurrent := GLGetProcAddress('glXMakeContextCurrent',@glCapXMakeContextCurrent);
  XGetCurrentReadDrawable := GLGetProcAddress('glXGetCurrentReadDrawable',@glCapXGetCurrentReadDrawable);
  XQueryContext := GLGetProcAddress('glXQueryContext',@glCapXQueryContext);
  XSelectEvent := GLGetProcAddress('glXSelectEvent',@glCapXSelectEvent);
  XGetSelectedEvent := GLGetProcAddress('glXGetSelectedEvent',@glCapXGetSelectedEvent);
  XBindTexImageARB := GLGetProcAddress('glXBindTexImageARB',@glCapXBindTexImageARB);
  XReleaseTexImageARB := GLGetProcAddress('glXReleaseTexImageARB',@glCapXReleaseTexImageARB);
  XDrawableAttribARB := GLGetProcAddress('glxDrawableAttribARB',@glCapXDrawableAttribARB);

  // GLX 1.4
  // GLX_ARB_create_context (EXT #56)
  XCreateContextAttribsARB := GLGetProcAddress('glXCreateContextAttribsARB',@glCapXCreateContextAttribsARB);

  // ###########################################################
  // locating functions and procedures for
  // Vendor/EXT WGL extensions
  // ###########################################################

  // WGL_EXT_swap_control (EXT #172)
  XSwapIntervalSGI := GLGetProcAddress('glXSwapIntervalSGI',@glCapXSwapIntervalSGI);
  XGetVideoSyncSGI := GLGetProcAddress('glXGetVideoSyncSGI',@glCapXGetVideoSyncSGI);
  XWaitVideoSyncSGI := GLGetProcAddress('glXWaitVideoSyncSGI',@glCapXWaitVideoSyncSGI);
  XFreeContextEXT := GLGetProcAddress('glXFreeContextEXT',@glCapXFreeContextEXT);
  XGetContextIDEXT := GLGetProcAddress('glXGetContextIDEXT',@glCapXGetContextIDEXT);
  XGetCurrentDisplayEXT := GLGetProcAddress('glXGetCurrentDisplayEXT',@glCapXGetCurrentDisplayEXT);
  XImportContextEXT := GLGetProcAddress('glXImportContextEXT',@glCapXImportContextEXT);
  XQueryContextInfoEXT := GLGetProcAddress('glXQueryContextInfoEXT',@glCapXQueryContextInfoEXT);
  XCopySubBufferMESA := GLGetProcAddress('glXCopySubBufferMESA',@glCapXCopySubBufferMESA);
  XCreateGLXPixmapMESA := GLGetProcAddress('glXCreateGLXPixmapMESA',@glCapXCreateGLXPixmapMESA);
  XReleaseBuffersMESA := GLGetProcAddress('glXReleaseBuffersMESA',@glCapXReleaseBuffersMESA);
  XSet3DfxModeMESA := GLGetProcAddress('glXSet3DfxModeMESA',@glCapXSet3DfxModeMESA);

  XBindTexImageEXT := GLGetProcAddress('glXBindTexImageEXT',@glCapXBindTexImageEXT);
  XReleaseTexImageEXT := GLGetProcAddress('glXReleaseTexImageEXT',@glCapXReleaseTexImageEXT);

  // GLX 1.4
  XMakeCurrentReadSGI := GLGetProcAddress('glXMakeCurrentReadSGI',@glCapXMakeCurrentReadSGI);
  XGetCurrentReadDrawableSGI := GLGetProcAddress('glXGetCurrentReadDrawableSGI',@glCapXGetCurrentReadDrawableSGI);
  XGetFBConfigAttribSGIX := GLGetProcAddress('glXGetFBConfigAttribSGIX',@glCapXGetFBConfigAttribSGIX);
  XChooseFBConfigSGIX := GLGetProcAddress('glXChooseFBConfigSGIX',@glCapXChooseFBConfigSGIX);
  XCreateGLXPixmapWithConfigSGIX := GLGetProcAddress('glXCreateGLXPixmapWithConfigSGIX',@glCapXCreateGLXPixmapWithConfigSGIX);
  XCreateContextWithConfigSGIX := GLGetProcAddress('glXCreateContextWithConfigSGIX',@glCapXCreateContextWithConfigSGIX);
  XGetVisualFromFBConfigSGIX := GLGetProcAddress('glXGetVisualFromFBConfigSGIX',@glCapXGetVisualFromFBConfigSGIX);
  XGetFBConfigFromVisualSGIX := GLGetProcAddress('glXGetFBConfigFromVisualSGIX',@glCapXGetFBConfigFromVisualSGIX);
  XCreateGLXPbufferSGIX := GLGetProcAddress('glXCreateGLXPbufferSGIX',@glCapXCreateGLXPbufferSGIX);
  XDestroyGLXPbufferSGIX := GLGetProcAddress('glXDestroyGLXPbufferSGIX',@glCapXDestroyGLXPbufferSGIX);
  XQueryGLXPbufferSGIX := GLGetProcAddress('glXQueryGLXPbufferSGIX',@glCapXQueryGLXPbufferSGIX);
  XSelectEventSGIX := GLGetProcAddress('glXSelectEventSGIX',@glCapXSelectEventSGIX);
  XGetSelectedEventSGIX := GLGetProcAddress('glXGetSelectedEventSGIX',@glCapXGetSelectedEventSGIX);
  XCushionSGI := GLGetProcAddress('glXCushionSGI',@glCapXCushionSGI);
  XBindChannelToWindowSGIX := GLGetProcAddress('glXBindChannelToWindowSGIX',@glCapXBindChannelToWindowSGIX);
  XChannelRectSGIX := GLGetProcAddress('glXChannelRectSGIX',@glCapXChannelRectSGIX);
  XQueryChannelRectSGIX := GLGetProcAddress('glXQueryChannelRectSGIX',@glCapXQueryChannelRectSGIX);
  XQueryChannelDeltasSGIX := GLGetProcAddress('glXQueryChannelDeltasSGIX',@glCapXQueryChannelDeltasSGIX);
  XChannelRectSyncSGIX := GLGetProcAddress('glXChannelRectSyncSGIX',@glCapXChannelRectSyncSGIX);
  XJoinSwapGroupSGIX := GLGetProcAddress('glXJoinSwapGroupSGIX',@glCapXJoinSwapGroupSGIX);
  XBindSwapBarrierSGIX := GLGetProcAddress('glXBindSwapBarrierSGIX',@glCapXBindSwapBarrierSGIX);
  XQueryMaxSwapBarriersSGIX := GLGetProcAddress('glXQueryMaxSwapBarriersSGIX',@glCapXQueryMaxSwapBarriersSGIX);
  XQueryHyperpipeNetworkSGIX := GLGetProcAddress('glXQueryHyperpipeNetworkSGIX',@glCapXQueryHyperpipeNetworkSGIX);

  XHyperpipeConfigSGIX := GLGetProcAddress('glXHyperpipeConfigSGIX',@glCapXHyperpipeConfigSGIX);
  XQueryHyperpipeConfigSGIX := GLGetProcAddress('glXQueryHyperpipeConfigSGIX',@glCapXQueryHyperpipeConfigSGIX);
  XDestroyHyperpipeConfigSGIX := GLGetProcAddress('glXDestroyHyperpipeConfigSGIX',@glCapXDestroyHyperpipeConfigSGIX);
  XBindHyperpipeSGIX := GLGetProcAddress('glXBindHyperpipeSGIX',@glCapXBindHyperpipeSGIX);
  XQueryHyperpipeBestAttribSGIX := GLGetProcAddress('glXQueryHyperpipeBestAttribSGIX',@glCapXQueryHyperpipeBestAttribSGIX);
  XHyperpipeAttribSGIX := GLGetProcAddress('glXHyperpipeAttribSGIX',@glCapXHyperpipeAttribSGIX);
  XQueryHyperpipeAttribSGIX := GLGetProcAddress('glXQueryHyperpipeAttribSGIX',@glCapXQueryHyperpipeAttribSGIX);
  XGetAGPOffsetMESA := GLGetProcAddress('glXGetAGPOffsetMESA',@glCapXGetAGPOffsetMESA);
  XEnumerateVideoDevicesNV := GLGetProcAddress('glXEnumerateVideoDevicesNV',@glCapXEnumerateVideoDevicesNV);
  XBindVideoDeviceNV := GLGetProcAddress('glXBindVideoDeviceNV',@glCapXBindVideoDeviceNV);
  XGetVideoDeviceNV := GLGetProcAddress('glXGetVideoDeviceNV',@glCapXGetVideoDeviceNV);
  XCopySubBufferMESA := GLGetProcAddress('glXCopySubBufferMESA',@glCapXCopySubBufferMESA);
  XReleaseBuffersMESA := GLGetProcAddress('glXReleaseBuffersMESA',@glCapXReleaseBuffersMESA);
  XCreateGLXPixmapMESA := GLGetProcAddress('glXCreateGLXPixmapMESA',@glCapXCreateGLXPixmapMESA);
  XSet3DfxModeMESA := GLGetProcAddress('glXSet3DfxModeMESA',@glCapXSet3DfxModeMESA);

  XAllocateMemoryNV := GLGetProcAddress('glXAllocateMemoryNV',@glCapXAllocateMemoryNV);
  XFreeMemoryNV := GLGetProcAddress('glXFreeMemoryNV',@glCapXFreeMemoryNV);

  XReleaseVideoDeviceNV := GLGetProcAddress('glXReleaseVideoDeviceNV',@glCapXReleaseVideoDeviceNV);
  XBindVideoImageNV := GLGetProcAddress('glXBindVideoImageNV',@glCapXBindVideoImageNV);
  XReleaseVideoImageNV := GLGetProcAddress('glXReleaseVideoImageNV',@glCapXReleaseVideoImageNV);
  XSendPbufferToVideoNV := GLGetProcAddress('glXSendPbufferToVideoNV',@glCapXSendPbufferToVideoNV);
  XGetVideoInfoNV := GLGetProcAddress('glXGetVideoInfoNV',@glCapXGetVideoInfoNV);
  XJoinSwapGroupNV := GLGetProcAddress('glXJoinSwapGroupNV',@glCapXJoinSwapGroupNV);
  XBindSwapBarrierNV := GLGetProcAddress('glXBindSwapBarrierNV',@glCapXBindSwapBarrierNV);
  XQuerySwapGroupNV := GLGetProcAddress('glXQuerySwapGroupNV',@glCapXQuerySwapGroupNV);
  XQueryMaxSwapGroupsNV := GLGetProcAddress('glXQueryMaxSwapGroupsNV',@glCapXQueryMaxSwapGroupsNV);
  XQueryFrameCountNV := GLGetProcAddress('glXQueryFrameCountNV',@glCapXQueryFrameCountNV);
  XResetFrameCountNV := GLGetProcAddress('glXResetFrameCountNV',@glCapXResetFrameCountNV);
  XBindVideoCaptureDeviceNV := GLGetProcAddress('glXBindVideoCaptureDeviceNV',@glCapXBindVideoCaptureDeviceNV);
  XEnumerateVideoCaptureDevicesNV := GLGetProcAddress('glXEnumerateVideoCaptureDevicesNV',@glCapXEnumerateVideoCaptureDevicesNV);
  XLockVideoCaptureDeviceNV := GLGetProcAddress('glxLockVideoCaptureDeviceNV',@glCapXLockVideoCaptureDeviceNV);
  XQueryVideoCaptureDeviceNV := GLGetProcAddress('glXQueryVideoCaptureDeviceNV',@glCapXQueryVideoCaptureDeviceNV);
  XReleaseVideoCaptureDeviceNV := GLGetProcAddress('glXReleaseVideoCaptureDeviceNV',@glCapXReleaseVideoCaptureDeviceNV);
  XSwapIntervalEXT := GLGetProcAddress('glXSwapIntervalEXT',@glCapXSwapIntervalEXT);
  XCopyImageSubDataNV := GLGetProcAddress('glXCopyImageSubDataNV',@glCapXCopyImageSubDataNV);
end;

{$ENDIF}

{$IFDEF DARWIN}
// ReadAGLImplementationProperties


procedure TGLExtensionsAndEntryPoints.ReadAGLImplementationProperties;
var
  MajorVersion, MinorVersion: integer;
begin
  // This procedure will probably need changing, as totally untested
  // This might only work if AGL functions/procedures are loaded dynamically
  if Assigned(GetString) then
    FBuffer := string(GetString(GL_EXTENSIONS))
  else
    FBuffer := '';

  A_aux_depth_stencil := CheckExtension('GL_APPLE_aux_depth_stencil');
  A_client_storage := CheckExtension('GL_APPLE_client_storage');
  A_element_array := CheckExtension('GL_APPLE_element_array');
  A_fence := CheckExtension('GL_APPLE_fence');
  A_float_pixels := CheckExtension('GL_APPLE_float_pixels');
  A_flush_buffer_range := CheckExtension('GL_APPLE_flush_buffer_range');
  A_flush_render := CheckExtension('GL_APPLE_flush_render');
  A_object_purgeable := CheckExtension('GL_APPLE_object_purgeable');
  A_packed_pixels := CheckExtension('GL_APPLE_packed_pixels');
  A_pixel_buffer := CheckExtension('GL_APPLE_pixel_buffer');
  A_rgb_422 := CheckExtension('GL_APPLE_rgb_422');
  A_specular_vector := CheckExtension('GL_APPLE_specular_vector');
  A_texture_range := CheckExtension('GL_APPLE_texture_range');
  A_transform_hint := CheckExtension('GL_APPLE_transform_hint');
  A_vertex_array_object := CheckExtension('GL_APPLE_vertex_array_object');
  A_vertex_array_range := CheckExtension('GL_APPLE_vertex_array_range');
  A_vertex_program_evaluators := CheckExtension('GL_APPLE_vertex_program_evaluators');
  A_ycbcr_422 := CheckExtension('GL_APPLE_ycbcr_422');
end;

procedure TGLExtensionsAndEntryPoints.ReadAGLExtensions;
begin
  // Managing pixel format object
  ACreatePixelFormat := AGLGetProcAddress('aglCreatePixelFormat',@glCapACreatePixelFormat);
  AChoosePixelFormat := AGLGetProcAddress('aglChoosePixelFormat',@glCapAChoosePixelFormat);
  ADestroyPixelFormat := AGLGetProcAddress('aglDestroyPixelFormat',@glCapADestroyPixelFormat);
  ADescribePixelFormat := AGLGetProcAddress('aglDescribePixelFormat',@glCapADescribePixelFormat);
  ADestroyPixelFormat := AGLGetProcAddress('aglDestroyPixelFormat',@glCapADestroyPixelFormat);
  AGetCGLPixelFormat := AGLGetProcAddress('aglGetCGLPixelFormat',@glCapAGetCGLPixelFormat);
  ADisplaysOfPixelFormat := AGLGetProcAddress('aglDisplaysOfPixelFormat',@glCapADisplaysOfPixelFormat);
  ANextPixelFormat := AGLGetProcAddress('aglNextPixelFormat',@glCapANextPixelFormat);
  // Managing context
  ACreateContext := AGLGetProcAddress('aglCreateContext',@glCapACreateContext);
  ACopyContext := AGLGetProcAddress('aglCopyContext',@glCapACopyContext);
  ADestroyContext := AGLGetProcAddress('aglDestroyContext',@glCapADestroyContext);
  AUpdateContext := AGLGetProcAddress('aglUpdateContext',@glCapAUpdateContext);
  ASetCurrentContext := AGLGetProcAddress('aglSetCurrentContext',@glCapASetCurrentContext);
  AGetCGLContext := AGLGetProcAddress('aglGetCGLContext',@glCapAGetCGLContext);
  AGetCurrentContext := AGLGetProcAddress('aglGetCurrentContext',@glCapAGetCurrentContext);
  ASwapBuffers := AGLGetProcAddress('aglSwapBuffers',@glCapASwapBuffers);
  AUpdateContext := AGLGetProcAddress('aglUpdateContext',@glCapAUpdateContext);
  // Managing Pixel Buffers
  ACreatePBuffer := AGLGetProcAddress('aglCreatePBuffer',@glCapACreatePBuffer);
  ADestroyPBuffer := AGLGetProcAddress('aglDestroyPBuffer',@glCapADestroyPBuffer);
  ADescribePBuffer := AGLGetProcAddress('aglDescribePBuffer',@glCapADescribePBuffer);
  AGetPBuffer := AGLGetProcAddress('aglGetPBuffer',@glCapAGetPBuffer);
  ASetPBuffer := AGLGetProcAddress('aglSetPBuffer',@glCapASetPBuffer);
  ATexImagePBuffer := AGLGetProcAddress('aglTexImagePBuffer',@glCapATexImagePBuffer);
  // Managing Drawable Objects
  ASetDrawable := AGLGetProcAddress('aglSetDrawable',@glCapASetDrawable);
  AGetDrawable := AGLGetProcAddress('aglGetDrawable',@glCapAGetDrawable);
  ASetFullScreen := AGLGetProcAddress('aglSetFullScreen',@glCapASetFullScreen);
  ASetOffScreen := AGLGetProcAddress('aglSetOffScreen',@glCapASetOffScreen);
  // Getting and Setting Context Options
  AEnable := AGLGetProcAddress('aglEnable',@glCapAEnable);
  ADisable := AGLGetProcAddress('aglDisable',@glCapADisable);
  AIsEnabled := AGLGetProcAddress('aglIsEnabled',@glCapAIsEnabled);
  ASetInteger := AGLGetProcAddress('aglSetInteger',@glCapASetInteger);
  AGetInteger := AGLGetProcAddress('aglGetInteger',@glCapAGetInteger);
  // Getting and Setting Global Information
  AConfigure := AGLGetProcAddress('aglConfigure',@glCapAConfigure);
  AGetVersion := AGLGetProcAddress('aglGetVersion',@glCapAGetVersion);
  AResetLibrary := AGLGetProcAddress('aglResetLibrary',@glCapAResetLibrary);
  // Getting Renderer Information
  ADescribeRenderer := AGLGetProcAddress('aglDescribeRenderer',@glCapADescribeRenderer);
  ADestroyRendererInfo := AGLGetProcAddress('aglDestroyRendererInfo',@glCapADestroyRendererInfo);
  ANextRendererInfo := AGLGetProcAddress('aglNextRendererInfo',@glCapANextRendererInfo);
  AQueryRendererInfoForCGDirectDisplayIDs := AGLGetProcAddress('aglQueryRendererInfoForCGDirectDisplayIDs',@glCapAQueryRendererInfoForCGDirectDisplayIDs);
  // Managing Virtual Screens
  AGetVirtualScreen := AGLGetProcAddress('aglGetVirtualScreen',@glCapAGetVirtualScreen);
  ASetVirtualScreen := AGLGetProcAddress('aglSetVirtualScreen',@glCapASetVirtualScreen);
  // Getting and Setting Windows
  ASetWindowRef := AGLGetProcAddress('aglSetWindowRef',@glCapASetWindowRef);
  AGetWindowRef := AGLGetProcAddress('aglGetWindowRef',@glCapAGetWindowRef);
  // Getting and Setting HIView Objects
  ASetHIViewRef := AGLGetProcAddress('aglSetHIViewRef',@glCapASetHIViewRef);
  AGetHIViewRef := AGLGetProcAddress('aglGetHIViewRef',@glCapAGetHIViewRef);
  // Getting Error Information
  AGetError := AGLGetProcAddress('aglGetError',@glCapAGetError);
  AErrorString := AGLGetProcAddress('aglErrorString',@glCapAErrorString);
end;
{$ENDIF}

// TrimAndSplitVersionString
//
procedure TrimAndSplitVersionString(buffer: string; var max, min: integer);
// Peels out the X.Y form from the given Buffer which must contain a version string like "text Minor.Major.Build text"
// at least however "Major.Minor".
var
  Separator: integer;
begin
  try
    // There must be at least one dot to separate major and minor version number.
    Separator := Pos('.', buffer);
    // At least one number must be before and one after the dot.
    if (Separator > 1) and (Separator < length(buffer)) and
      (AnsiChar(buffer[Separator - 1]) in ['0' .. '9']) and
      (AnsiChar(buffer[Separator + 1]) in ['0' .. '9']) then
    begin
      // OK, it's a valid version string. Now remove unnecessary parts.
      Dec(Separator);
      // Find last non-numeric character before version number.
      while (Separator > 0) and (AnsiChar(buffer[Separator]) in ['0' .. '9']) do
        Dec(Separator);
      // Delete leading characters which do not belong to the version string.
      Delete(buffer, 1, Separator);
      Separator := Pos('.', buffer) + 1;
      // Find first non-numeric character after version number
      while (Separator <= length(buffer)) and
        (AnsiChar(buffer[Separator]) in ['0' .. '9']) do
        Inc(Separator);
      // delete trailing characters not belonging to the version string
      Delete(buffer, Separator, 255);
      // Now translate the numbers.
      Separator := Pos('.', buffer);
      // This is necessary because the buffer length might have changed.
      max := StrToInt(Copy(buffer, 1, Separator - 1));
      min := StrToInt(Copy(buffer, Separator + 1, 255));
    end
    else
      Abort;
  except
    min := 0;
    max := 0;
  end;
end;

function IsVersionMet(MajorVersion, MinorVersion, actualMajorVersion,
  actualMinorVersion: integer): boolean;
begin
  Result := (actualMajorVersion > MajorVersion) or
    ((actualMajorVersion = MajorVersion) and (actualMinorVersion >= MinorVersion));
end;

// InitOpenGL


function InitOpenGL: boolean;
begin
{$IFNDEF GLS_OPENGL_ES}
  if (GLHandle = INVALID_MODULEHANDLE) or (GLUHandle = INVALID_MODULEHANDLE) then
    Result := InitOpenGLFromLibrary(opengl32, glu32)
  else
    Result := True;
{$ELSE}
  {$IFNDEF DARWIN}
  EGLHandle := LoadLibrary(PChar(libEGL));
  Result := EGLHandle <> INVALID_MODULEHANDLE;
  {$ENDIF}
  GLES1Handle := LoadLibrary(PChar(libGLES1));
  GLES2Handle := LoadLibrary(PChar(libGLES2));
  Result := Result and (EGLHandle <> INVALID_MODULEHANDLE);
  if Result then
  begin
{$IFDEF GLS_OPENGL_ES}
    eglGetError := EGLGetProcAddress('eglGetError');
    eglGetDisplay := EGLGetProcAddress('eglGetDisplay');
    eglInitialize := EGLGetProcAddress('eglInitialize');
    eglTerminate := EGLGetProcAddress('eglTerminate');
    eglQueryString := EGLGetProcAddress('eglQueryString');
    eglGetConfigs := EGLGetProcAddress('eglGetConfigs');
    eglChooseConfig := EGLGetProcAddress('eglChooseConfig');
    eglGetConfigAttrib := EGLGetProcAddress('eglGetConfigAttrib');
    eglCreateWindowSurface := EGLGetProcAddress('eglCreateWindowSurface');
    eglCreatePbufferSurface := EGLGetProcAddress('eglCreatePbufferSurface');
    eglCreatePixmapSurface := EGLGetProcAddress('eglCreatePixmapSurface');
    eglDestroySurface := EGLGetProcAddress('eglDestroySurface');
    eglQuerySurface := EGLGetProcAddress('eglQuerySurface');
    eglBindAPI := EGLGetProcAddress('eglBindAPI');
    eglQueryAPI := EGLGetProcAddress('eglQueryAPI');
    eglWaitClient := EGLGetProcAddress('eglWaitClient');
    eglReleaseThread := EGLGetProcAddress('eglReleaseThread');
    eglCreatePbufferFromClientBuffer := EGLGetProcAddress('eglCreatePbufferFromClientBuffer');
    eglSurfaceAttrib := EGLGetProcAddress('eglSurfaceAttrib');
    eglBindTexImage := EGLGetProcAddress('eglBindTexImage');
    eglReleaseTexImage := EGLGetProcAddress('eglReleaseTexImage');
    eglSwapInterval := EGLGetProcAddress('eglSwapInterval');
    eglCreateContext := EGLGetProcAddress('eglCreateContext');
    eglDestroyContext := EGLGetProcAddress('eglDestroyContext');
    eglMakeCurrent := EGLGetProcAddress('eglMakeCurrent');
    eglGetCurrentContext := EGLGetProcAddress('eglGetCurrentContext');
    eglGetCurrentSurface := EGLGetProcAddress('eglGetCurrentSurface');
    eglGetCurrentDisplay := EGLGetProcAddress('eglGetCurrentDisplay');
    eglQueryContext := EGLGetProcAddress('eglQueryContext');
    eglWaitGL := EGLGetProcAddress('eglWaitGL');
    eglWaitNative := EGLGetProcAddress('eglWaitNative');
    eglSwapBuffers := EGLGetProcAddress('eglSwapBuffers');
    eglCopyBuffers := EGLGetProcAddress('eglCopyBuffers');
{$ENDIF}
  end;
{$ENDIF}
end;

// InitOpenGLFromLibrary


function InitOpenGLFromLibrary(const GLName, GLUName: string): boolean;
begin
  Result := False;
  CloseOpenGL;

  GLHandle := LoadLibrary(PChar(GLName));
  GLUHandle := LoadLibrary(PChar(GLUName));
{$IFDEF DARWIN}
  AGLHandle := LoadLibrary(PChar(libAGL));
  dlHandle := LoadLibrary(PChar(libdl));
{$ENDIF}

  if (GLHandle <> INVALID_MODULEHANDLE) and (GLUHandle <> INVALID_MODULEHANDLE) then
  begin
    Result := True;
  end
  else
    CloseOpenGL;
end;

// IsOpenGLInitialized


function IsOpenGLInitialized: boolean;
begin
  Result :={$IFNDEF GLS_OPENGL_ES}(GLHandle <> INVALID_MODULEHANDLE){$ELSE}
  ((GLES2Handle <> INVALID_MODULEHANDLE)or (GLES1Handle <> INVALID_MODULEHANDLE)){$ENDIF};
end;

// CloseOpenGL


procedure CloseOpenGL;
begin
{$IFNDEF GLS_OPENGL_ES}
  if GLHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(GLHandle);
    GLHandle := INVALID_MODULEHANDLE;
  end;

  if GLUHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(GLUHandle);
    GLUHandle := INVALID_MODULEHANDLE;
  end;

  {$IFDEF DARWIN}
  if AGLHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(AGLHandle);
    AGLHandle := INVALID_MODULEHANDLE;
  end;
  if dlHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(dlHandle);
    dlHandle := INVALID_MODULEHANDLE;
  end;
  {$ENDIF}

{$ELSE}
  {$IFNDEF DARWIN}
  if EGLHandle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(EGLHandle);
    EGLHandle := INVALID_MODULEHANDLE;
  end;
  {$ENDIF}
  if GLES2Handle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(GLES2Handle);
    GLES2Handle := INVALID_MODULEHANDLE;
  end;
  if GLES1Handle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(GLES1Handle);
    GLES1Handle := INVALID_MODULEHANDLE;
  end;
{$ENDIF}
end;

// UnloadOpenGL


procedure UnloadOpenGL;
begin
  CloseOpenGL;
end;

// LoadOpenGL


function LoadOpenGL: boolean;
begin
  Result := InitOpenGL;
end;

// LoadOpenGLFromLibrary


function LoadOpenGLFromLibrary(GLName, GLUName: string): boolean;
begin
  Result := InitOpenGLFromLibrary(GLName, GLUName);
end;

// IsOpenGLLoaded


function IsOpenGLLoaded: boolean;
begin
  Result := IsOpenGLInitialized();
end;

// IsMesaGL


function IsMesaGL: boolean;
begin
  Result := GLGetProcAddress('glResizeBuffersMESA', nil) <> nil;
end;

initialization

{$IFDEF FPC}
  { according to bug 7570, this is necessary on all x86 platforms,
    maybe we've to fix the sse control word as well }
  { Yes, at least for darwin/x86_64 (JM) }
  {$IF DEFINED(cpui386) or DEFINED(cpux86_64)}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
    exOverflow, exUnderflow, exPrecision]);
  {$IFEND}
{$ELSE}
  Set8087CW($133F);
{$ENDIF}

finalization

  CloseOpenGL;

end.
