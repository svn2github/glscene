//
// GLScene on Vulkan, http://glscene.sourceforge.net
//
{
  OpenGL adapter
}
unit GLS.OpenGLAdapter;

interface

{$I GLScene.inc}

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.OpenGL,
  Winapi.OpenGLext,

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
  System.SysUtils,
  GLS.Log,
  GLS.VectorGeometry,
  GLS.Strings,
  GLS.VectorTypes;

type
  PHGPUNV = ^HGPUNV;
  HGPUNV = THandle;

  PGPUDevice = ^TGPUDevice;
  TGPUDevice = record
    cb: Cardinal;
    DeviceName: array[0..31] of AnsiChar;
    DeviceString: array[0..127] of AnsiChar;
    Flags: Cardinal;
    rcVirtualScreen: TRect;
  end;

   // WGL_NV_video_capture
  HVIDEOINPUTDEVICENV = THandle;
  PHVIDEOINPUTDEVICENV = ^HVIDEOINPUTDEVICENV;

  TVKVectord3  = array[0..2] of GLdouble;
  TVKArrayd3 = TVKVectord3;

type
   // GLU types
   TVKUNurbs = record end;
   TVKUQuadric = record end;
   TVKUTesselator = record end;

   PGLUNurbs = ^TVKUNurbs;
   PGLUQuadric = ^TVKUQuadric;
   PGLUTesselator=  ^TVKUTesselator;

   // backwards compatibility
   TVKUNurbsObj = TVKUNurbs;
   TVKUQuadricObj = TVKUQuadric;
   TVKUTesselatorObj = TVKUTesselator;
   TVKUTriangulatorObj = TVKUTesselator;

   PGLUNurbsObj = PGLUNurbs;
   PGLUQuadricObj = PGLUQuadric;
   PGLUTesselatorObj = PGLUTesselator;
   PGLUTriangulatorObj = PGLUTesselator;

var
  // supported versions
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
  GL_VERSION_4_1,
  GL_VERSION_4_2,
  GL_VERSION_4_3,
  GL_VERSION_4_4,
  GL_VERSION_4_5,
  GLU_VERSION_1_1,
  GLU_VERSION_1_2,
  GLU_VERSION_1_3: Boolean;

  // ARB approved OpenGL extensions in sorted order
  GL_3DFX_multisample,
  GL_3DFX_tbuffer,
  GL_3DFX_texture_compression_FXT1,
  GL_AMD_conservative_depth,
  GL_AMD_debug_output,
  GL_AMD_depth_clamp_separate,
  GL_AMD_draw_buffers_blend,
  GL_AMD_name_gen_delete,
  GL_AMD_performance_monitor,
  GL_AMD_pinned_memory,
  GL_AMD_query_buffer_object,
  GL_AMD_seamless_cubemap_per_texture,
  GL_AMD_shader_stencil_export,
  GL_AMD_stencil_operation_extended,
  GL_AMD_texture_texture4,
  GL_AMD_transform_feedback3_lines_triangles,
  GL_AMD_vertex_shader_layer,
  GL_AMD_vertex_shader_tesselator,
  GL_AMD_vertex_shader_tessellator,
  GL_AMD_vertex_shader_viewport_index,
  GL_AMDX_debug_output,
  GL_APPLE_aux_depth_stencil,
  GL_APPLE_client_storage,
  GL_APPLE_element_array,
  GL_APPLE_fence,
  GL_APPLE_float_pixels,
  GL_APPLE_object_purgeable,
  GL_APPLE_rgb_422,
  GL_APPLE_row_bytes,
  GL_APPLE_specular_vector,
  GL_APPLE_texture_range,
  GL_APPLE_transform_hint,
  GL_APPLE_vertex_array_object,
  GL_APPLE_vertex_array_range,
  GL_APPLE_vertex_program_evaluators,
  GL_APPLE_ycbcr_422,
  GL_ARB_arrays_of_arrays,
  GL_ARB_base_instance,
  GL_ARB_bindless_texture,
  GL_ARB_blend_func_extended,
  GL_ARB_buffer_storage,
  GL_ARB_cl_event,
  GL_ARB_clear_buffer_object,
  GL_ARB_clear_texture,
  GL_ARB_clip_control,
  GL_ARB_color_buffer_float,
  GL_ARB_compatibility,
  GL_ARB_compressed_texture_pixel_storage,
  GL_ARB_compute_shader,
  GL_ARB_compute_variable_group_size,
  GL_ARB_conditional_render_inverted,
  GL_ARB_conservative_depth,
  GL_ARB_copy_buffer,
  GL_ARB_copy_image,
  GL_ARB_cull_distance,
  GL_ARB_debug_group,
  GL_ARB_debug_label,
  GL_ARB_debug_output,
  GL_ARB_debug_output2,
  GL_ARB_depth_buffer_float,
  GL_ARB_depth_clamp,
  GL_ARB_depth_texture,
  GL_ARB_derivative_control,
  GL_ARB_direct_state_access,
  GL_ARB_draw_buffers,
  GL_ARB_draw_buffers_blend,
  GL_ARB_draw_elements_base_vertex,
  GL_ARB_draw_indirect,
  GL_ARB_draw_instanced,
  GL_ARB_enhanced_layouts,
  GL_ARB_ES2_compatibility,
  GL_ARB_ES3_1_compatibility,
  GL_ARB_ES3_compatibility,
  GL_ARB_explicit_attrib_location,
  GL_ARB_explicit_uniform_location,
  GL_ARB_fragment_coord_conventions,
  GL_ARB_fragment_layer_viewport,
  GL_ARB_fragment_program,
  GL_ARB_fragment_program_shadow,
  GL_ARB_fragment_shader,
  GL_ARB_framebuffer_no_attachments,
  GL_ARB_framebuffer_object,
  GL_ARB_framebuffer_sRGB,
  GL_ARB_geometry_shader4,
  GL_ARB_get_program_binary,
  GL_ARB_get_texture_sub_image,
  GL_ARB_gpu_shader_fp64,
  GL_ARB_gpu_shader5,
  GL_ARB_half_float_pixel,
  GL_ARB_half_float_vertex,
  GL_ARB_imaging,
  GL_ARB_instanced_arrays,
  GL_ARB_internalformat_query,
  GL_ARB_internalformat_query2,
  GL_ARB_invalidate_subdata,
  GL_ARB_map_buffer_alignment,
  GL_ARB_map_buffer_range,
  GL_ARB_matrix_palette,
  GL_ARB_multi_bind,
  GL_ARB_multi_draw_indirect,
  GL_ARB_multisample,
  GL_ARB_multitexture,
  GL_ARB_occlusion_query,
  GL_ARB_occlusion_query2,
  GL_ARB_pixel_buffer_object,
  GL_ARB_point_parameters,
  GL_ARB_point_sprite,
  GL_ARB_program_interface_query,
  GL_ARB_provoking_vertex,
  GL_ARB_query_buffer_object,
  GL_ARB_robust_buffer_access_behavior,
  GL_ARB_robustness,
  GL_ARB_robustness_isolation,
  GL_ARB_sample_shading,
  GL_ARB_sampler_objects,
  GL_ARB_seamless_cube_map,
  GL_ARB_separate_shader_objects,
  GL_ARB_shader_atomic_counters,
  GL_ARB_shader_bit_encoding,
  GL_ARB_shader_image_load_store,
  GL_ARB_shader_image_size,
  GL_ARB_shader_objects,
  GL_ARB_shader_precision,
  GL_ARB_shader_stencil_export,
  GL_ARB_shader_storage_buffer_object,
  GL_ARB_shader_subroutine,
  GL_ARB_shader_texture_image_samples,
  GL_ARB_shader_texture_lod,
  GL_ARB_shading_language_100,
  GL_ARB_shading_language_420pack,
  GL_ARB_shading_language_include,
  GL_ARB_shading_language_packing,
  GL_ARB_shadow,
  GL_ARB_shadow_ambient,
  GL_ARB_sparse_texture,
  GL_ARB_stencil_texturing,
  GL_ARB_sync,
  GL_ARB_tessellation_shader,
  GL_ARB_texture_barrier,
  GL_ARB_texture_border_clamp,
  GL_ARB_texture_buffer_object,
  GL_ARB_texture_buffer_object_rgb32,
  GL_ARB_texture_buffer_range,
  GL_ARB_texture_compression,
  GL_ARB_texture_compression_bptc,
  GL_ARB_texture_compression_rgtc,
  GL_ARB_texture_cube_map,
  GL_ARB_texture_cube_map_array,
  GL_ARB_texture_env_add,
  GL_ARB_texture_env_combine,
  GL_ARB_texture_env_crossbar,
  GL_ARB_texture_env_dot3,
  GL_ARB_texture_float,
  GL_ARB_texture_gather,
  GL_ARB_texture_mirror_clamp_to_edge,
  GL_ARB_texture_mirrored_repeat,
  GL_ARB_texture_multisample,
  GL_ARB_texture_non_power_of_two,
  GL_ARB_texture_query_levels,
  GL_ARB_texture_query_lod,
  GL_ARB_texture_rectangle,
  GL_ARB_texture_rg,
  GL_ARB_texture_rgb10_a2ui,
  GL_ARB_texture_stencil8,
  GL_ARB_texture_storage,
  GL_ARB_texture_storage_multisample,
  GL_ARB_texture_swizzle,
  GL_ARB_texture_view,
  GL_ARB_timer_query,
  GL_ARB_transform_feedback_instanced,
  GL_ARB_transform_feedback2,
  GL_ARB_transform_feedback3,
  GL_ARB_transpose_matrix,
  GL_ARB_uniform_buffer_object,
  GL_ARB_vertex_array_bgra,
  GL_ARB_vertex_array_object,
  GL_ARB_vertex_attrib_64bit,
  GL_ARB_vertex_attrib_binding,
  GL_ARB_vertex_blend,
  GL_ARB_vertex_buffer_object,
  GL_ARB_vertex_program,
  GL_ARB_vertex_shader,
  GL_ARB_vertex_type_10f_11f_11f_rev,
  GL_ARB_vertex_type_2_10_10_10_rev,
  GL_ARB_viewport_array,
  GL_ARB_window_pos,
  GL_ATI_draw_buffers,
  GL_ATI_element_array,
  GL_ATI_envmap_bumpmap,
  GL_ATI_fragment_shader,
  GL_ATI_map_object_buffer,
  GL_ATI_meminfo,
  GL_ATI_pn_triangles,
  GL_ATI_separate_stencil,
  GL_ATI_text_fragment_shader,
  GL_ATI_texture_compression_3dc,
  GL_ATI_texture_env_combine3,
  GL_ATI_texture_float,
  GL_ATI_texture_mirror_once,
  GL_ATI_vertex_array_object,
  GL_ATI_vertex_attrib_array_object,
  GL_ATI_vertex_streams,
  GL_EXT_422_pixels,
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
  GL_EXT_cmyka,
  GL_EXT_color_matrix,
  GL_EXT_color_subtable,
  GL_EXT_compiled_vertex_array,
  GL_EXT_convolution,
  GL_EXT_coordinate_frame,
  GL_EXT_copy_texture,
  GL_EXT_cull_vertex,
  GL_EXT_depth_bounds_test,
  GL_EXT_direct_state_access,
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
  GL_EXT_histogram,
  GL_EXT_index_array_formats,
  GL_EXT_index_func,
  GL_EXT_index_material,
  GL_EXT_index_texture,
  GL_EXT_light_texture,
  GL_EXT_misc_attribute,
  GL_EXT_multi_draw_arrays,
  GL_EXT_multisample,
  GL_EXT_packed_depth_stencil,
  GL_EXT_packed_float,
  GL_EXT_packed_pixels,
  GL_EXT_paletted_texture,
  GL_EXT_pixel_buffer_object,
  GL_EXT_pixel_transform,
  GL_EXT_pixel_transform_color_table,
  GL_EXT_point_parameters,
  GL_EXT_polygon_offset,
  GL_EXT_provoking_vertex,
  GL_EXT_rescale_normal,
  GL_EXT_secondary_color,
  GL_EXT_separate_shader_objects,
  GL_EXT_separate_specular_color,
  GL_EXT_shader_image_load_store,
  GL_EXT_shadow_funcs,
  GL_EXT_shared_texture_palette,
  GL_EXT_stencil_clear_tag,
  GL_EXT_stencil_two_side,
  GL_EXT_stencil_wrap,
  GL_EXT_subtexture,
  GL_EXT_texture,
  GL_EXT_texture_array,
  GL_EXT_texture_buffer_object,
  GL_EXT_texture_compression_dxt1,
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
  GL_EXT_texture_perturb_normal,
  GL_EXT_texture_rectangle,
  GL_EXT_texture_shared_exponent,
  GL_EXT_texture_snorm,
  GL_EXT_texture_sRGB,
  GL_EXT_texture_sRGB_decode,
  GL_EXT_texture_swizzle,
  GL_EXT_texture3D,
  GL_EXT_timer_query,
  GL_EXT_transform_feedback,
  GL_EXT_vertex_array,
  GL_EXT_vertex_array_bgra,
  GL_EXT_vertex_attrib_64bit,
  GL_EXT_vertex_shader,
  GL_EXT_vertex_weighting,
  GL_FfdMaskSGIX,
  GL_GREMEDY_frame_terminator,
  GL_GREMEDY_string_marker,
  GL_HP_convolution_border_modes,
  GL_HP_image_transform,
  GL_HP_occlusion_test,
  GL_HP_texture_lighting,
  GL_IBM_cull_vertex,
  GL_IBM_multimode_draw_arrays,
  GL_IBM_rasterpos_clip,
  GL_IBM_texture_mirrored_repeat,
  GL_IBM_vertex_array_lists,
  GL_INGR_blend_func_separate,
  GL_INGR_color_clamp,
  GL_INGR_interlace_read,
  GL_INGR_palette_buffer,
  GL_INTEL_parallel_arrays,
  GL_INTEL_texture_scissor,
  GL_KHR_blend_equation_advanced,
  GL_KHR_blend_equation_advanced_coherent,
  GL_KHR_context_flush_control,
  GL_KHR_debug,
  GL_KHR_robust_buffer_access_behavior,
  GL_KHR_robustness,
  GL_KTX_buffer_region,
  GL_MESA_resize_buffers,
  GL_MESA_window_pos,
  GL_NV_bindless_texture,
  GL_NV_blend_square,
  GL_NV_conditional_render,
  GL_NV_copy_depth_to_color,
  GL_NV_copy_image,
  GL_NV_depth_buffer_float,
  GL_NV_depth_clamp,
  GL_NV_evaluators,
  GL_NV_explicit_multisample,
  GL_NV_fence,
  GL_NV_float_buffer,
  GL_NV_fog_distance,
  GL_NV_fragment_program,
  GL_NV_fragment_program_option,
  GL_NV_fragment_program2,
  GL_NV_fragment_program4,
  GL_NV_framebuffer_multisample_coverage,
  GL_NV_geometry_program4,
  GL_NV_gpu_program4,
  GL_NV_gpu_program5,
  GL_NV_gpu_shader5,
  GL_NV_half_float,
  GL_NV_light_max_exponent,
  GL_NV_multisample_coverage,
  GL_NV_multisample_filter_hint,
  GL_NV_occlusion_query,
  GL_NV_packed_depth_stencil,
  GL_NV_parameter_buffer_object,
  GL_NV_parameter_buffer_object2,
  GL_NV_path_rendering,
  GL_NV_pixel_data_range,
  GL_NV_point_sprite,
  GL_NV_present_video,
  GL_NV_primitive_restart,
  GL_NV_register_combiners,
  GL_NV_register_combiners2,
  GL_NV_shader_atomic_float,
  GL_NV_shader_buffer_load,
  GL_NV_shader_buffer_store,
  GL_NV_tessellation_program5,
  GL_NV_texgen_emboss,
  GL_NV_texgen_reflection,
  GL_NV_texture_barrier,
  GL_NV_texture_compression_vtc,
  GL_NV_texture_env_combine4,
  GL_NV_texture_expand_normal,
  GL_NV_texture_rectangle,
  GL_NV_texture_shader,
  GL_NV_texture_shader2,
  GL_NV_texture_shader3,
  GL_NV_transform_feedback,
  GL_NV_transform_feedback2,
  GL_NV_vdpau_interop,
  GL_NV_vertex_array_range,
  GL_NV_vertex_array_range2,
  GL_NV_vertex_attrib_integer_64bit,
  GL_NV_vertex_buffer_unified_memory,
  GL_NV_vertex_program,
  GL_NV_vertex_program1_1,
  GL_NV_vertex_program2,
  GL_NV_vertex_program2_option,
  GL_NV_vertex_program3,
  GL_NV_vertex_program4,
  GL_NV_video_capture,
  GL_NVX_gpu_memory_info,
  GL_OML_interlace,
  GL_OML_resample,
  GL_OML_subsample,
  GL_PGI_misc_hints,
  GL_PGI_vertex_hints,
  GL_REND_screen_coordinates,
  GL_S3_s3tc,
  GL_SGI_color_matrix,
  GL_SGI_color_table,
  GL_SGI_depth_pass_instrument,
  GL_SGI_texture_color_table,
  GL_SGIS_detail_texture,
  GL_SGIS_fog_function,
  GL_SGIS_generate_mipmap,
  GL_SGIS_multisample,
  GL_SGIS_pixel_texture,
  GL_SGIS_point_line_texgen,
  GL_SGIS_point_parameters,
  GL_SGIS_sharpen_texture,
  GL_SGIS_texture_border_clamp,
  GL_SGIS_texture_color_mask,
  GL_SGIS_texture_edge_clamp,
  GL_SGIS_texture_filter4,
  GL_SGIS_texture_lod,
  GL_SGIS_texture_select,
  GL_SGIS_texture4D,
  GL_SGIX_async,
  GL_SGIX_async_histogram,
  GL_SGIX_async_pixel,
  GL_SGIX_blend_alpha_minmax,
  GL_SGIX_calligraphic_fragment,
  GL_SGIX_clipmap,
  GL_SGIX_convolution_accuracy,
  GL_SGIX_depth_pass_instrument,
  GL_SGIX_depth_texture,
  GL_SGIX_flush_raster,
  GL_SGIX_fog_offset,
  GL_SGIX_fog_scale,
  GL_SGIX_fragment_lighting,
  GL_SGIX_framezoom,
  GL_SGIX_igloo_interface,
  GL_SGIX_impact_pixel_texture,
  GL_SGIX_instruments,
  GL_SGIX_interlace,
  GL_SGIX_ir_instrument1,
  GL_SGIX_list_priority,
  GL_SGIX_pixel_texture,
  GL_SGIX_pixel_tiles,
  GL_SGIX_polynomial_ffd,
  GL_SGIX_reference_plane,
  GL_SGIX_resample,
  GL_SGIX_scalebias_hint,
  GL_SGIX_shadow,
  GL_SGIX_shadow_ambient,
  GL_SGIX_sprite,
  GL_SGIX_subsample,
  GL_SGIX_tag_sample_buffer,
  GL_SGIX_texture_add_env,
  GL_SGIX_texture_coordinate_clamp,
  GL_SGIX_texture_lod_bias,
  GL_SGIX_texture_multi_buffer,
  GL_SGIX_texture_scale_bias,
  GL_SGIX_texture_select,
  GL_SGIX_vertex_preclip,
  GL_SGIX_ycrcb,
  GL_SGIX_ycrcb_subsample,
  GL_SGIX_ycrcba,
  GL_SUN_convolution_border_modes,
  GL_SUN_global_alpha,
  GL_SUN_mesh_array,
  GL_SUN_slice_accum,
  GL_SUN_triangle_list,
  GL_SUN_vertex,
  GL_SUNX_constant_data,
  GL_WIN_phong_shading,
  GL_WIN_specular_fog,
  GLX_ARB_context_flush_control,
  GLX_ARB_create_context,
  GLX_ARB_create_context_profile,
  GLX_ARB_create_context_robustness,
  GLX_ARB_fbconfig_float,
  GLX_ARB_framebuffer_sRGB,
  GLX_ARB_get_proc_address,
  GLX_ARB_multisample,
  GLX_ARB_vertex_buffer_object,
  GLX_EXT_create_context_es2_profile,
  GLX_EXT_fbconfig_packed_float,
  GLX_EXT_framebuffer_sRGB,
  GLX_EXT_import_context,
  GLX_EXT_swap_control,
  GLX_EXT_texture_from_pixmap,
  GLX_EXT_visual_info,
  GLX_EXT_visual_rating,
  GLX_VERSION_1_3,
  GLX_VERSION_1_4,
  WGL_3DFX_multisample,
  WGL_3DL_stereo_control,
  WGL_AMD_gpu_association,
  WGL_ARB_buffer_region,
  WGL_ARB_context_flush_control,
  WGL_ARB_create_context,
  WGL_ARB_create_context_profile,
  WGL_ARB_create_context_robustness,
  WGL_ARB_extensions_string,
  WGL_ARB_framebuffer_sRGB,
  WGL_ARB_make_current_read,
  WGL_ARB_multisample,
  WGL_ARB_pbuffer,
  WGL_ARB_pixel_format,
  WGL_ARB_pixel_format_float,
  WGL_ARB_render_texture,
  WGL_ATI_pixel_format_float,
  WGL_EXT_create_context_es2_profile,
  WGL_EXT_depth_float,
  WGL_EXT_display_color_table,
  WGL_EXT_extensions_string,
  WGL_EXT_framebuffer_sRGB,
  WGL_EXT_make_current_read,
  WGL_EXT_multisample,
  WGL_EXT_pbuffer,
  WGL_EXT_pixel_format,
  WGL_EXT_pixel_format_packed_float,
  WGL_EXT_swap_control,
  WGL_I3D_digital_video_control,
  WGL_I3D_gamma,
  WGL_I3D_genlock,
  WGL_I3D_image_buffer,
  WGL_I3D_swap_frame_lock,
  WGL_I3D_swap_frame_usage,
  WGL_NV_copy_image,
  WGL_NV_DX_interop,
  WGL_NV_DX_interop2,
  WGL_NV_float_buffer,
  WGL_NV_gpu_affinity,
  WGL_NV_multisample_coverage,
  WGL_NV_present_video,
  WGL_NV_render_depth_texture,
  WGL_NV_render_texture_rectangle,
  WGL_NV_swap_group,
  WGL_NV_vertex_array_range,
  WGL_NV_video_capture,
  WGL_NV_video_output,
  WGL_OML_sync_control,
  WIN_draw_range_elements,
  WIN_swap_hint : Boolean;

type
  EOpenGLError = class(Exception);

  TVKExtensionsAndEntryPoints = class
  private
    FBuffer: string;
    FInitialized: boolean;
    FDebug: boolean;
    FDebugIds: PGLuint;
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
{$IFDEF EGL_SUPPORT}
    procedure ReadEGLExtensions;
    procedure ReadEGLImplementationProperties;
{$ENDIF}
    function GetAddress(ProcName: string): Pointer;
    function GetAddressNoSuffixes(ProcName: string): Pointer;
    function GetAddressAlt(ProcName1, ProcName2: string): Pointer;
    function GetCapAddress: Pointer;
  public

{$IFDEF SUPPORT_WGL}
    wglCreateBufferRegionARB: function(DC: HDC; iLayerPlane: Integer; uType: GLenum) : Integer; stdcall;
    wglDeleteBufferRegionARB: procedure(hRegion: Integer); stdcall;
    wglSaveBufferRegionARB: function(hRegion: Integer; x, y, width, height: Integer): BOOL; stdcall;
    wglRestoreBufferRegionARB: function(hRegion: Integer; x, y, width, height: Integer;
                                      xSrc, ySrc: Integer): BOOL; stdcall;
    wglGetExtensionsStringARB: function(DC: HDC): PGLChar; stdcall;
    wglGetPixelFormatAttribivARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: GLenum;
                     const piAttributes: PGLint; piValues : PGLint) : BOOL; stdcall;
    wglGetPixelFormatAttribfvARB: function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: GLenum;
                     const piAttributes: PGLint; piValues: PGLFloat) : BOOL; stdcall;
    wglChoosePixelFormatARB: function(DC: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLFloat;
                     nMaxFormats: GLuint; piFormats: PGLint; nNumFormats: PGLenum) : BOOL; stdcall;
    wglMakeContextCurrentARB: function(hDrawDC: HDC; hReadDC: HDC; _hglrc: HGLRC): BOOL; stdcall;
    wglGetCurrentReadDCARB: function(): HDC; stdcall;
    wglCreatePbufferARB: function(DC: HDC; iPixelFormat: GLInt; iWidth, iHeight : GLInt;
                       const piAttribList: PGLint) : GLInt; stdcall;
    wglGetPbufferDCARB: function(hPbuffer: GLInt) : HDC; stdcall;
    wglReleasePbufferDCARB: function(hPbuffer: GLInt; DC: HDC) : Integer; stdcall;
    wglDestroyPbufferARB: function(hPbuffer: GLInt): BOOL; stdcall;
    wglQueryPbufferARB: function(hPbuffer: GLInt; iAttribute : Integer;
                      piValue: PGLint) : BOOL; stdcall;
    wglBindTexImageARB: function(hPbuffer: GLInt; iBuffer: Integer): BOOL; stdcall;
    wglReleaseTexImageARB: function(hpBuffer: GLInt; iBuffer: Integer): BOOL; stdcall;
    wglSetPbufferAttribARB: function(hpBuffer: GLInt; const piAttribList: PGLint): BOOL; stdcall;
    wglCreateContextAttribsARB: function(DC: HDC; hShareContext: HGLRC;
           attribList: PGLint):HGLRC; stdcall;
    wglSwapIntervalEXT: function(interval : Integer) : BOOL; stdcall;
    wglGetSwapIntervalEXT: function(): GLint; stdcall;
    wglEnumGpusNV: function(iGpuIndex: Cardinal; var hGpu: HGPUNV): Boolean; stdcall;
    wglEnumGpuDevicesNV: function(hGpu: HGPUNV; iDeviceIndex: Cardinal; lpGpuDevice: PGPU_DEVICE): Boolean; stdcall;
    wglCreateAffinityDCNV: function(const phGpuList: PHGPUNV): HDC; stdcall;
    wglEnumGpusFromAffinityDCNV: function(hAffinityDC: HDC; iGpuIndex: Cardinal; hGpu: PHGPUNV): Boolean; stdcall;
    wglDeleteDCNV: function(hDC: HDC): Boolean; stdcall;
    wglBindVideoCaptureDeviceNV: function(uVideoSlot: Cardinal; hDevice: HVIDEOINPUTDEVICENV): Boolean; stdcall;
    wglEnumerateVideoCaptureDevicesNV: function(hDc: HDC; phDeviceList: PHVIDEOINPUTDEVICENV): Cardinal; stdcall;
    wglLockVideoCaptureDeviceNV: function(hDc: HDC; hDevice: HVIDEOINPUTDEVICENV): Boolean; stdcall;
    wglQueryVideoCaptureDeviceNV: function(hDc: HDC; hDevice: HVIDEOINPUTDEVICENV; iAttribute: Integer; piValue: PInteger): Boolean; stdcall;
    wglReleaseVideoCaptureDeviceNV: function(hDc: HDC; hDevice: HVIDEOINPUTDEVICENV): Boolean; stdcall;
    wglCopyImageSubDataNV: function(hSrcRc: HGLRC; srcName: GLuint; srcTarget: GLenum; srcLevel: GLint; srcX: GLint; srcY: GLint; srcZ: GLint; hDstRC: HGLRC; dstName: GLuint; dstTarget: GLenum; dstLevel: GLint; dstX: GLint; dstY: GLint;
          dstZ: GLint; width: GLsizei; height: GLsizei; depth: GLsizei): Boolean; stdcall;
    wglDXSetResourceShareHandleNV: function(dxObject : Pointer; hareHandle : Cardinal) : Boolean; stdcall;
    wglDXOpenDeviceNV: function(dxDevice : Pointer) : Cardinal; stdcall;
    wglDXCloseDeviceNV: function(hDevice : Cardinal) : Boolean; stdcall;
    wglDXRegisterObjectNV: function(hDevice : Cardinal; dxObject : Pointer; name : GLUInt; _type : GLEnum; access : GLenum) : Cardinal; stdcall;
    wglDXUnregisterObjectNV: function(hDevice : Cardinal; hObject : Cardinal) : Boolean; stdcall;
    wglDXObjectAccessNV: function(hObject : Cardinal; access : GLenum) : Boolean; stdcall;
    wglDXLockObjectsNV: function(hDevice : Cardinal; count : GLint; hObjects : PCardinal) : Boolean; stdcall;
    wglDXUnlockObjectsNV: function (hDevice : Cardinal; count : GLint; hObjects : PCardinal) : Boolean; stdcall;
    wglAllocateMemoryNV: procedure(size: GLsizei; readfreq: GLfloat; writefreq: GLfloat; priority: GLfloat); stdcall;
    wglFreeMemoryNV: procedure(_pointer: Pointer); stdcall;
{$ENDIF}

{$IFDEF SUPPORT_GLX}
    //---------------------------------------------------------------------
    // function and procedure definitions for
    // ARB approved GLX extensions
    //---------------------------------------------------------------------
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
    ADescribePixelFormat : function(pix: TAGLPixelFormat; attrib: GLint; value: PGLint): GLboolean; cdecl;
    AGetCGLPixelFormat : function(pix: TAGLPixelFormat; cgl_pix: Pointer): GLboolean; cdecl;
    ADisplaysOfPixelFormat : function(pix: TAGLPixelFormat; ndevs: PGLint): CGDirectDisplayID; cdecl;
    ANextPixelFormat : function(pix: TAGLPixelFormat): TAGLPixelFormat; cdecl;
    // Managing context
    ACreateContext: function(pix: TAGLPixelFormat; share: TAGLContext): TAGLContext; cdecl;
    ACopyContext : function(src: TAGLContext; dst: TAGLContext; mask: GLuint): GLboolean; cdecl;
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
    ADescribePBuffer : function(pbuffer: TAGLPbuffer; width, height: PGLint; target: PGLenum; internalFormat: PGLenum; max_level: PGLint): GLboolean; cdecl;
    AGetPBuffer : function(ctx: TAGLContext; out pbuffer: TAGLPbuffer; face, level, screen: PGLint): GLboolean; cdecl;
    ASetPBuffer : function(ctx: TAGLContext; pbuffer: TAGLPbuffer; face: GLint; level: GLint; screen: GLint): GLboolean; cdecl;
    ATexImagePBuffer : function(ctx: TAGLContext; pbuffer: TAGLPbuffer; source: GLint): GLboolean; cdecl;
    // Managing Drawable Objects
    ASetDrawable: function(ctx: TAGLContext; draw: TAGLDrawable): GLboolean; cdecl; // deprecated
    AGetDrawable: function(ctx: TAGLContext): TAGLDrawable; cdecl; // deprecated
    ASetFullScreen: function(ctx: TAGLContext; Width: GLsizei; Height: GLsizei; freq: GLsizei;
      device: GLint): GLboolean; cdecl;
    ASetOffScreen : function(ctx: TAGLContext; width, height, rowbytes: GLsizei; out baseaddr: Pointer): GLboolean; cdecl;
    // Getting and Setting Context Options
    AEnable : function(ctx: TAGLContext; pname: GLenum): GLboolean; cdecl;
    ADisable : function(ctx: TAGLContext; pname: GLenum): GLboolean; cdecl;
    AIsEnabled : function(ctx: TAGLContext; pname: GLenum): GLboolean; cdecl;
    ASetInteger : function(ctx: TAGLContext; pname: GLenum; params: PGLint): GLboolean; cdecl;
    AGetInteger : function(ctx: TAGLContext; pname: GLenum; params: PGLint): GLboolean; cdecl;
    // Getting and Setting Global Information
    AConfigure : function(pname: GLenum; param: GLuint): GLboolean; cdecl;
    AGetVersion : procedure(major: PGLint; minor: PGLint); cdecl;
    AResetLibrary : procedure(); cdecl;
    // Getting Renderer Information
    ADescribeRenderer : function(rend: TAGLRendererInfo; prop: GLint; value: PGLint): GLboolean; cdecl;
    ADestroyRendererInfo : procedure(rend: TAGLRendererInfo); cdecl;
    ANextRendererInfo : function(rend: TAGLRendererInfo): TAGLRendererInfo; cdecl;
    AQueryRendererInfoForCGDirectDisplayIDs : function(dspIDs: CGDirectDisplayID; ndev: GLint): TAGLRendererInfo; cdecl;
    // Managing Virtual Screens
    AGetVirtualScreen : function(ctx: TAGLContext): GLint; cdecl;
    ASetVirtualScreen : function(ctx: TAGLContext; screen: GLint): GLboolean; cdecl;
    // Getting and Setting Windows
    ASetWindowRef : function(ctx: TAGLContext; window: WindowRef): GLboolean; cdecl;
    AGetWindowRef : function(ctx: TAGLContext): GLint; cdecl;
    // Getting and Setting HIView Objects
    ASetHIViewRef : function(ctx: TAGLContext; hiview: HIViewRef): GLboolean; cdecl;
    AGetHIViewRef : function(ctx: TAGLContext): HIViewRef; cdecl;
    // Getting Error Information
    AGetError : function(): GLenum; cdecl;
    AErrorString : function(code: GLenum): PGLChar; cdecl;
{$ENDIF}

{$IFDEF EGL_SUPPORT}
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
    OES_vertex_half_float: Boolean;

{ Functions }
    EGetError : function:EGLint;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EGetDisplay : function(display_id:EGLNativeDisplayType):EGLDisplay;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EInitialize : function(dpy:EGLDisplay; major:pEGLint; minor:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ETerminate : function(dpy:EGLDisplay):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EQueryString : function(dpy:EGLDisplay; name:EGLint):pchar;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EGetConfigs : function(dpy:EGLDisplay; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EChooseConfig : function(dpy:EGLDisplay; attrib_list:pEGLint; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EGetConfigAttrib : function(dpy:EGLDisplay; config:EGLConfig; attribute:EGLint; value:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ECreateWindowSurface : function(dpy:EGLDisplay; config:EGLConfig; win:EGLNativeWindowType; attrib_list:pEGLint):EGLSurface;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ECreatePbufferSurface : function(dpy:EGLDisplay; config:EGLConfig; attrib_list:pEGLint):EGLSurface;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ECreatePixmapSurface : function(dpy:EGLDisplay; config:EGLConfig; pixmap:EGLNativePixmapType; attrib_list:pEGLint):EGLSurface;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EDestroySurface : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EQuerySurface : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EBindAPI : function(api:EGLenum):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EQueryAPI : function:EGLenum;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EWaitClient : function:EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EReleaseThread : function:EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ECreatePbufferFromClientBuffer : function(dpy:EGLDisplay; buftype:EGLenum; buffer:EGLClientBuffer; config:EGLConfig; attrib_list:pEGLint):EGLSurface;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ESurfaceAttrib : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:EGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EBindTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EReleaseTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ESwapInterval : function(dpy:EGLDisplay; interval:EGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ECreateContext : function(dpy:EGLDisplay; config:EGLConfig; share_context:EGLContext; attrib_list:pEGLint):EGLContext;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EDestroyContext : function(dpy:EGLDisplay; ctx:EGLContext):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EMakeCurrent : function(dpy:EGLDisplay; draw:EGLSurface; read:EGLSurface; ctx:EGLContext):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EGetCurrentContext : function:EGLContext;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EGetCurrentSurface : function(readdraw:EGLint):EGLSurface;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EGetCurrentDisplay : function:EGLDisplay;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EQueryContext : function(dpy:EGLDisplay; ctx:EGLContext; attribute:EGLint; value:pEGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EWaitGL : function:EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    EWaitNative : function(engine:EGLint):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ESwapBuffers : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
    ECopyBuffers : function(dpy:EGLDisplay; surface:EGLSurface; target:EGLNativePixmapType):EGLBoolean;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}

{$ENDIF EGL_SUPPORT}
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

{$IFDEF SUPPORT_GLX}
// GLX 1.0
function glXGetProcAddress(const Name: PAnsiChar): Pointer; cdecl; external opengl32;
function glXGetProcAddressARB(const Name: PAnsiChar): Pointer; cdecl; external opengl32;
function glXChooseVisual(dpy: PDisplay; screen: GLint;
  attribList: PGLint): PXVisualInfo; cdecl; external opengl32;
function glXCreateContext(dpy: PDisplay; vis: PXVisualInfo;
  shareList: GLXContext; direct: GLboolean): GLXContext; cdecl; external opengl32;
procedure glXDestroyContext(dpy: PDisplay; ctx: GLXContext); cdecl; external opengl32;
function glXMakeCurrent(dpy: PDisplay; drawable: GLXDrawable;
  ctx: GLXContext): GLboolean; cdecl; external opengl32;
procedure glXCopyContext(dpy: PDisplay; src: GLXContext; dst: GLXContext; mask: GLuint);
  cdecl; external opengl32;
procedure glXSwapBuffers(dpy: PDisplay; drawable: GLXDrawable); cdecl; external opengl32;
function glXCreateGLXPixmap(dpy: PDisplay; visual: PXVisualInfo;
  pixmap: GLXPixmap): GLXPixmap; cdecl; external opengl32;
procedure glXDestroyGLXPixmap(dpy: PDisplay; pixmap: GLXPixmap);
  cdecl; external opengl32;
function glXQueryExtension(dpy: PDisplay; errorb: PGLint; event: PGLint): GLboolean;
  cdecl; external opengl32;
function glXQueryVersion(dpy: PDisplay; maj: PGLint; min: PGLint): GLboolean;
  cdecl; external opengl32;
function glXIsDirect(dpy: PDisplay; ctx: GLXContext): GLboolean;
  cdecl; external opengl32;
function glXGetConfig(dpy: PDisplay; visual: PXVisualInfo; attrib: GLint;
  Value: PGLint): GLint; cdecl; external opengl32;
function glXGetCurrentContext: GLXContext; cdecl; external opengl32;
function glXGetCurrentDrawable: GLXDrawable; cdecl; external opengl32;
procedure glXWaitGL; cdecl; external opengl32;
procedure glXWaitX; cdecl; external opengl32;
procedure glXUseXFont(font: XFont; First: GLint; Count: GLint; list: GLint);
  cdecl; external opengl32;
function glXQueryExtensionsString(dpy: PDisplay; screen: GLint): PGLChar;
  cdecl; external opengl32;
function glXQueryServerString(dpy: PDisplay; screen: GLint; Name: GLint): PGLChar;
  cdecl; external opengl32;
function glXGetClientString(dpy: PDisplay; Name: GLint): PGLChar;
  cdecl; external opengl32;
function glXGetCurrentDisplay: PDisplay; cdecl; external opengl32;
{$ENDIF}

{$IFDEF DARWIN}
function aglChoosePixelFormat(gdevs: PAGLDevice; ndev: GLint; attribs: PGLint): TAGLPixelFormat; cdecl; external libAGL;
procedure aglDestroyPixelFormat(pix: TAGLPixelFormat); cdecl; external libAGL;
function aglCreateContext(pix: TAGLPixelFormat; share: TAGLContext): TAGLContext; cdecl; external libAGL;
function aglDestroyContext(ctx: TAGLContext): GLboolean; cdecl; external libAGL;
function aglSetCurrentContext (ctx: TAGLContext): GLboolean; cdecl; external libAGL;
function aglSetDrawable (ctx: TAGLContext; draw: TAGLDrawable): GLboolean; cdecl; external libAGL;
{$ENDIF}

function GLLibGetProcAddress(ProcName: PGLChar): Pointer;
function GLGetProcAddress(ProcName: PGLChar): Pointer;
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

//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------
implementation
//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------

const
  glPrefix = 'gl';

// ************** Windows specific ********************
{$IFDEF MSWINDOWS}

const
  INVALID_MODULEHANDLE = 0;

var
  GLHandle: HINST;
  GLUHandle: HINST;
{$IFDEF EGL_SUPPORT}
  EGLHandle: HINST;
  EGL2Handle: HINST;
{$ENDIF}

function GLGetProcAddress(ProcName: PGLChar): Pointer;
begin
{$IFNDEF EGL_SUPPORT}
  Result := wglGetProcAddress(ProcName);
{$ELSE}
  Result := GetProcAddress(EGL2Handle, ProcName);
{$ENDIF}
end;

{$IFDEF EGL_SUPPORT}
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
{$IFDEF EGL_SUPPORT}
  EGLHandle: TLibHandle = 0;
  EGL2Handle: TLibHandle = 0;
{$ENDIF}

{$IFDEF DARWIN}
  AGLHandle: TLibHandle = 0;
  dlHandle: TLibHandle = 0;
  {$IFDEF EGL_SUPPORT}
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

function GLGetProcAddress(ProcName: PGLChar): Pointer;
begin
{$IFNDEF EGL_SUPPORT}
  {$IFDEF SUPPORT_GLX}
  if @glXGetProcAddress <> nil then
    Result := glXGetProcAddress(ProcName);

  if Result <> nil then
    exit;

  if @glXGetProcAddressARB <> nil then
    Result := glXGetProcAddressARB(ProcName);

  if Result <> nil then
    exit;
  {$ENDIF}
  Result := GetProcAddress(GLHandle, ProcName);
{$ELSE}
  Result := GetProcAddress(EGL2Handle, ProcName);
{$ENDIF}
end;

{$IFDEF DARWIN}
function AGLGetProcAddress(ProcName: PGLChar): Pointer;
begin
  Result := GetProcAddress(AGLHandle, ProcName);
end;
{$ELSE}

{$IFDEF EGL_SUPPORT}
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
  {$IFDEF GLS_LOGGING}
   if length > 0 then
      GLSLogger.LogDebug(string(message));
  {$ENDIF}
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

constructor TVKExtensionsAndEntryPoints.Create;
begin
  FInitialized := False;
end;

procedure glCap;
{$IFDEF MSWINDOWS} stdcall;
{$ENDIF}{$IFDEF UNIX} cdecl;
{$ENDIF}
begin
 {$IFDEF GLS_LOGGING}
  GLSLogger.LogError('Call OpenGL function with undefined entry point');
  {$ENDIF}
  Abort;
end;

function TVKExtensionsAndEntryPoints.GetAddress(ProcName: string): Pointer;
var
  vName: string;
begin
  vName := glPrefix + ProcName;
  Result := GLGetProcAddress(PGLChar(vName));
  {$IFDEF DARWIN}
  if Result = nil then
  begin
    Result := AGLGetProcAddress(PGLChar(String(vName)));
    if Result = nil then
    begin
      vName := glPrefix + ProcName + 'APPLE';
      Result := GLGetProcAddress(PGLChar(String(vName)));
  {$ENDIF}
  if Result = nil then
  begin
    vName := glPrefix + ProcName + 'ARB';
    Result := GLGetProcAddress(PGLChar(vName));
    if Result = nil then
    begin
      vName := glPrefix + ProcName;
      Result := GLLibGetProcAddress(PGLChar(vName));
      if Result = nil then
      begin
        vName := glPrefix + ProcName + 'EXT';
        Result := GLGetProcAddress(PGLChar(vName));
        if Result = nil then
        begin
          vName := glPrefix + ProcName + 'NV';
          Result := GLGetProcAddress(PGLChar(vName));
          if Result = nil then
          begin
            vName := glPrefix + ProcName + 'ATI';
            Result := GLGetProcAddress(PGLChar(vName));
            if Result = nil then
            begin
              vName := glPrefix + ProcName + 'OES';
              Result := GLGetProcAddress(PGLChar(vName));
              if Result = nil then
                Result := @glCap;
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
  if Result <> @glCap then
    GLSLogger.LogDebug('Finded entry point of ' + vName)
  else
    GLSLogger.LogDebug('Can''t find entry point of ' + vName);
{$ENDIF}
end;

function TVKExtensionsAndEntryPoints.GetAddressAlt(ProcName1, ProcName2:
  string): Pointer;
begin
  Result := GetAddress(ProcName1);
  if Result = @glCap then
    Result := GetAddress(ProcName2);
end;

function TVKExtensionsAndEntryPoints.GetAddressNoSuffixes(ProcName: string): Pointer;
var
  vName: string;
begin
  vName := glPrefix + ProcName;
  Result := GLGetProcAddress(PGLChar(vName));
  if Result = nil then
    Result := @glCap;
{$IFDEF GLS_OPENGL_DEBUG}
  if Result <> @glCap then
    GLSLogger.LogDebug('Finded entry point of ' + vName)
  else
    GLSLogger.LogDebug('Can''t find entry point of ' + vName);
{$ENDIF}
end;

function TVKExtensionsAndEntryPoints.GetCapAddress: Pointer;
begin
  Result := @glCap;
end;

procedure TVKExtensionsAndEntryPoints.CheckError;
var
  glError: GLuint;
  Count: word;
begin
  if FInitialized then
    try
      glError := glGetError();
      if glError <> GL_NO_ERROR then
      begin
        Count := 0;
        try
          while (glGetError <> GL_NO_ERROR) and (Count < 6) do
            Inc(Count);
        except
        end;
        if not (FDebug and GL_ARB_debug_output) then
          case glError of
            GL_INVALID_ENUM:
              GLSLogger.LogError(format(glsOpenGLError, ['Invalid enum']));
            GL_INVALID_VALUE:
              GLSLogger.LogError(format(glsOpenGLError, ['Invalid value']));
            GL_INVALID_OPERATION:
              GLSLogger.LogError(format(glsOpenGLError, ['Invalid Operation']));
            GL_OUT_OF_MEMORY:
              GLSLogger.LogError(format(glsOpenGLError, ['Out of memory']));
          end;
      end;
    except
      GLSLogger.LogError(format(glsOpenGLError, ['Exception in glGetError']));
    end;
end;

procedure TVKExtensionsAndEntryPoints.ClearError;
var
  n: integer;
begin
  n := 0;
  while (glGetError <> GL_NO_ERROR) and (n < 6) do
    Inc(n);
end;

function TVKExtensionsAndEntryPoints.CheckExtension(const Extension: string): boolean;
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

procedure TVKExtensionsAndEntryPoints.Initialize(ATemporary: boolean);
var
  i: integer;
  numExt: GLint;
  MajorVersion, MinorVersion: integer;
  name: GLenum;
  pname: GLenum;
  params: PGLint;
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
{$IFDEF EGL_SUPPORT}
  ReadEGLExtensions;
  ReadEGLImplementationProperties;
{$ENDIF}
  glGetString(name); // := GetAddress('GetString');
  glGetStringi := GetAddress('GetStringi');
  glGetIntegerv(pname, params); // : = GetAddress('GetIntegerv');
  glGetError; // := GetAddress('GetError');
  // determine OpenGL versions supported
  FBuffer := string(glGetString(GL_VERSION));
  TrimAndSplitVersionString(FBuffer, MajorVersion, MinorVersion);
  GL_VERSION_1_0 := True;
  GL_VERSION_1_1 := IsVersionMet(1, 1, MajorVersion, MinorVersion);
  GL_VERSION_1_2 := IsVersionMet(1, 2, MajorVersion, MinorVersion);
  GL_VERSION_1_3 := IsVersionMet(1, 3, MajorVersion, MinorVersion);
  GL_VERSION_1_4 := IsVersionMet(1, 4, MajorVersion, MinorVersion);
  GL_VERSION_1_5 := IsVersionMet(1, 5, MajorVersion, MinorVersion);
  GL_VERSION_2_0 := IsVersionMet(2, 0, MajorVersion, MinorVersion);
  GL_VERSION_2_1 := IsVersionMet(2, 1, MajorVersion, MinorVersion);
  GL_VERSION_3_0 := IsVersionMet(3, 0, MajorVersion, MinorVersion);
  GL_VERSION_3_1 := IsVersionMet(3, 1, MajorVersion, MinorVersion);
  GL_VERSION_3_2 := IsVersionMet(3, 2, MajorVersion, MinorVersion);
  GL_VERSION_3_3 := IsVersionMet(3, 3, MajorVersion, MinorVersion);
  GL_VERSION_4_0 := IsVersionMet(4, 0, MajorVersion, MinorVersion);
  GL_VERSION_4_1 := IsVersionMet(4, 1, MajorVersion, MinorVersion);
  GL_VERSION_4_2 := IsVersionMet(4, 2, MajorVersion, MinorVersion);
  GL_VERSION_4_3 := IsVersionMet(4, 3, MajorVersion, MinorVersion);
  GL_VERSION_4_4 := IsVersionMet(4, 4, MajorVersion, MinorVersion);
  GL_VERSION_4_5 := IsVersionMet(4, 5, MajorVersion, MinorVersion);

  if vNotInformed then
  begin
    GLSLogger.LogNotice('');
    GLSLogger.LogInfo('OpenGL rendering context information:');
    GLSLogger.LogInfo(format('Renderer     : %s', [glGetString(GL_RENDERER)]));
    GLSLogger.LogInfo(format('Vendor       : %s', [glGetString(GL_VENDOR)]));
    GLSLogger.LogInfo(format('Version      : %s', [glGetString(GL_VERSION)]));
    if GL_VERSION_2_0 then
      GLSLogger.LogInfo(format('GLSL version : %s',
        [glGetString(GL_SHADING_LANGUAGE_VERSION)]))
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
  if GL_VERSION_3_0 then
  begin
    FBuffer := '';
    glGetIntegerv(GL_NUM_EXTENSIONS, @numExt);
    for i := 0 to numExt - 1 do
      FBuffer := FBuffer + string(glGetStringi(GL_EXTENSIONS, i)) + ' ';
  end
  else
    FBuffer := string(glGetString(GL_EXTENSIONS));
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
  GL_ARB_matrix_palette := CheckExtension('GL_ARB_matrix_palette');
  GL_ARB_multisample := CheckExtension('GL_ARB_multisample');
  // ' ' to avoid collision with WGL variant
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
  GL_ARB_texture_buffer_object_rgb32 :=
    CheckExtension('GL_ARB_texture_buffer_object_rgb32');
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
  GL_ARB_get_program_binary := CheckExtension('GL_ARB_get_program_binary');
  GL_ARB_separate_shader_objects := CheckExtension('GL_ARB_separate_shader_objects');
  GL_ARB_shader_stencil_export := CheckExtension('GL_ARB_shader_stencil_export');
  GL_KHR_debug := CheckExtension('GL_KHR_debug');
  GL_ARB_clear_buffer_object := CheckExtension('GL_ARB_clear_buffer_object');
  GL_ARB_compute_shader := CheckExtension('GL_ARB_compute_shader');
  GL_ARB_copy_image := CheckExtension('GL_ARB_copy_image');
  GL_ARB_debug_group := CheckExtension('GL_ARB_debug_group');
  GL_ARB_debug_label := CheckExtension('GL_ARB_debug_label');
  GL_ARB_debug_output2 := CheckExtension('GL_ARB_debug_output2');
  GL_ARB_ES3_compatibility := CheckExtension('GL_ARB_ES3_compatibility');
  GL_ARB_explicit_uniform_location := CheckExtension('GL_ARB_explicit_uniform_location');
  GL_ARB_fragment_layer_viewport := CheckExtension('GL_ARB_fragment_layer_viewport');
  GL_ARB_framebuffer_no_attachments := CheckExtension('GL_ARB_framebuffer_no_attachments');
  GL_ARB_internalformat_query2 := CheckExtension('GL_ARB_internalformat_query2');
  GL_ARB_invalidate_subdata := CheckExtension('GL_ARB_invalidate_subdata');
  GL_ARB_multi_draw_indirect := CheckExtension('GL_ARB_multi_draw_indirect');
  GL_ARB_program_interface_query := CheckExtension('GL_ARB_program_interface_query');
  GL_ARB_shader_image_size := CheckExtension('GL_ARB_shader_image_size');
  GL_ARB_shader_storage_buffer_object := CheckExtension('GL_ARB_shader_storage_buffer_object');
  GL_ARB_stencil_texturing := CheckExtension('GL_ARB_stencil_texturing');
  GL_ARB_texture_buffer_range := CheckExtension('GL_ARB_texture_buffer_range');
  GL_ARB_texture_query_levels := CheckExtension('GL_ARB_texture_query_levels');
  GL_ARB_texture_storage_multisample := CheckExtension('GL_ARB_texture_storage_multisample');
  GL_ARB_texture_view := CheckExtension('GL_ARB_texture_view');
  GL_ARB_vertex_attrib_binding := CheckExtension('GL_ARB_vertex_attrib_binding');
  GL_ARB_robustness_isolation := CheckExtension('GL_ARB_robustness_isolation');
  GL_ARB_cl_event := CheckExtension('GL_ARB_cl_event');
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
  GL_EXT_stencil_two_side := CheckExtension('GL_EXT_stencil_two_side');
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
  GL_EXT_texture_sRGB_decode := CheckExtension('GL_EXT_texture_sRGB_decode');
  GL_EXT_direct_state_access := CheckExtension('EXT_direct_state_access');
  GL_EXT_texture_swizzle := CheckExtension('EXT_texture_swizzle');

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
  GL_NV_multisample_filter_hint := CheckExtension('GL_NV_multisample_filter_hint');
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
  GL_NV_vertex_buffer_unified_memory :=
    CheckExtension('GL_NV_vertex_buffer_unified_memory');
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
  GL_ATI_meminfo := CheckExtension('GL_ATI_meminfo');
  GL_NVX_gpu_memory_info := CheckExtension('GL_NVX_gpu_memory_info');
  GL_NV_vdpau_interop := CheckExtension('GL_NV_vdpau_interop');
  GL_NV_path_rendering := CheckExtension('GL_NV_path_rendering');

  GL_GREMEDY_frame_terminator := CheckExtension('GL_GREMEDY_frame_terminator');
  GL_GREMEDY_string_marker := CheckExtension('GL_GREMEDY_string_marker');
  GL_AMDX_debug_output := CheckExtension('AMDX_debug_output');
  GL_ARB_debug_output := CheckExtension('GL_ARB_debug_output');

{$IFDEF LINUX}
  VDPAUInitNV := GetAddressNoSuffixes('VDPAUInitNV');
  VDPAUFiniNV := GetAddressNoSuffixes('VDPAUFiniNV');
  VDPAURegisterVideoSurfaceNV := GetAddressNoSuffixes('VDPAURegisterVideoSurfaceNV');
  VDPAURegisterOutputSurfaceNV := GetAddressNoSuffixes('VDPAURegisterOutputSurfaceNV');
  VDPAUIsSurfaceNV := GetAddressNoSuffixes('VDPAUIsSurfaceNV');
  VDPAUUnregisterSurfaceNV := GetAddressNoSuffixes('VDPAUUnregisterSurfaceNV');
  VDPAUGetSurfaceivNV := GetAddressNoSuffixes('VDPAUGetSurfaceivNV');
  VDPAUSurfaceAccessNV := GetAddressNoSuffixes('VDPAUSurfaceAccessNV');
  VDPAUMapSurfacesNV := GetAddressNoSuffixes('VDPAUMapSurfacesNV');
  VDPAUUnmapSurfacesNV := GetAddressNoSuffixes('VDPAUUnmapSurfacesNV');
{$ENDIF LINUX}

  if FDebug then
    if GL_ARB_debug_output then
    begin
      glDebugMessageCallback(DebugCallBack, nil);
      glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, FDebugIds, 1{True});
      glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
    end
    else if GL_AMDX_debug_output then
    begin
    {
      glDebugMessageCallbackAMDX(DebugCallBackAMD, nil);
      glDebugMessageEnableAMDX(0, 0, 0, FDebugIds, True);
     }
    end
    else
      FDebug := False;

  SetLength(FBuffer, 0);

  InitOpenGLext;  //< Winapi.OpenGLext

  FInitialized := True;
end;

procedure TVKExtensionsAndEntryPoints.Close;
begin
  if FDebug then
    if GL_ARB_debug_output then
    begin
      glDebugMessageCallback(nil, nil);
      glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, FDebugIds, 0{False});
    end
    else
    if GL_AMDX_debug_output then
    begin
      {
      glDebugMessageCallbackAMDX(nil, nil);
      glDebugMessageEnableAMDX(0, 0, 0, FDebugIds, False);
      }
    end;

  GL_VERSION_1_0 := False;
  GL_VERSION_1_1 := False;
  GL_VERSION_1_2 := False;
  GL_VERSION_1_3 := False;
  GL_VERSION_1_4 := False;
  GL_VERSION_1_5 := False;
  GL_VERSION_2_0 := False;
  GL_VERSION_2_1 := False;
  GL_VERSION_3_0 := False;
  GL_VERSION_3_1 := False;
  GL_VERSION_3_2 := False;
  GL_VERSION_3_3 := False;
  GL_VERSION_4_0 := False;
  GL_VERSION_4_1 := False;
  GL_VERSION_4_2 := False;
  GL_VERSION_4_3 := False;
  GL_VERSION_4_4 := False;
  GL_VERSION_4_5 := False;

  //sorted consts
  GL_3DFX_multisample := False;
  GL_3DFX_tbuffer := False;
  GL_3DFX_texture_compression_FXT1 := False;
  GL_AMD_vertex_shader_tessellator := False;
  GL_ARB_blend_func_extended := False;
  GL_ARB_cl_event := False;
  GL_ARB_clear_buffer_object := False;
  GL_ARB_color_buffer_float := False;
  GL_ARB_compatibility := False;
  GL_ARB_compute_shader := False;
  GL_ARB_copy_buffer := False;
  GL_ARB_copy_image := False;
  GL_ARB_debug_group := False;
  GL_ARB_debug_label := False;
  GL_ARB_debug_output := False;
  GL_ARB_debug_output2 := False;
  GL_ARB_depth_buffer_float := False;
  GL_ARB_depth_clamp := False;
  GL_ARB_depth_texture := False;
  GL_ARB_draw_buffers := False;
  GL_ARB_draw_buffers_blend := False;
  GL_ARB_draw_elements_base_vertex := False;
  GL_ARB_draw_indirect := False;
  GL_ARB_draw_instanced := False;
  GL_ARB_ES3_compatibility := False;
  GL_ARB_explicit_attrib_location := False;
  GL_ARB_explicit_uniform_location := False;
  GL_ARB_fragment_coord_conventions := False;
  GL_ARB_fragment_layer_viewport := False;
  GL_ARB_fragment_program := False;
  GL_ARB_fragment_program_shadow := False;
  GL_ARB_fragment_shader := False;
  GL_ARB_framebuffer_no_attachments := False;
  GL_ARB_framebuffer_object := False;
  GL_ARB_framebuffer_sRGB := False;
  GL_ARB_geometry_shader4 := False;
  GL_ARB_get_program_binary := False;
  GL_ARB_gpu_shader_fp64 := False;
  GL_ARB_gpu_shader5 := False;
  GL_ARB_half_float_pixel := False;
  GL_ARB_half_float_vertex := False;
  GL_ARB_imaging := False;
  GL_ARB_instanced_arrays := False;
  GL_ARB_internalformat_query2 := False;
  GL_ARB_invalidate_subdata := False;
  GL_ARB_map_buffer_range := False;
  GL_ARB_matrix_palette := False;
  GL_ARB_multi_draw_indirect := False;
  GL_ARB_multisample := False;
  GL_ARB_multitexture := False;
  GL_ARB_occlusion_query := False;
  GL_ARB_occlusion_query2 := False;
  GL_ARB_pixel_buffer_object := False;
  GL_ARB_point_parameters := False;
  GL_ARB_point_sprite := False;
  GL_ARB_program_interface_query := False;
  GL_ARB_provoking_vertex := False;
  GL_ARB_robustness_isolation := False;
  GL_ARB_sample_shading := False;
  GL_ARB_sampler_objects := False;
  GL_ARB_seamless_cube_map := False;
  GL_ARB_separate_shader_objects := False;
  GL_ARB_shader_bit_encoding := False;
  GL_ARB_shader_image_size := False;
  GL_ARB_shader_objects := False;
  GL_ARB_shader_stencil_export := False;
  GL_ARB_shader_storage_buffer_object := False;
  GL_ARB_shader_subroutine := False;
  GL_ARB_shader_texture_lod := False;
  GL_ARB_shading_language_100 := False;
  GL_ARB_shadow := False;
  GL_ARB_shadow_ambient := False;
  GL_ARB_stencil_texturing := False;
  GL_ARB_sync := False;
  GL_ARB_tessellation_shader := False;
  GL_ARB_texture_border_clamp := False;
  GL_ARB_texture_buffer_object := False;
  GL_ARB_texture_buffer_object_rgb32 := False;
  GL_ARB_texture_buffer_range := False;
  GL_ARB_texture_compression := False;
  GL_ARB_texture_compression_bptc := False;
  GL_ARB_texture_compression_rgtc := False;
  GL_ARB_texture_cube_map := False;
  GL_ARB_texture_cube_map_array := False;
  GL_ARB_texture_env_add := False;
  GL_ARB_texture_env_combine := False;
  GL_ARB_texture_env_crossbar := False;
  GL_ARB_texture_env_dot3 := False;
  GL_ARB_texture_float := False;
  GL_ARB_texture_gather := False;
  GL_ARB_texture_mirrored_repeat := False;
  GL_ARB_texture_multisample := False;
  GL_ARB_texture_non_power_of_two := False;
  GL_ARB_texture_query_levels := False;
  GL_ARB_texture_query_lod := False;
  GL_ARB_texture_rectangle := False;
  GL_ARB_texture_rg := False;
  GL_ARB_texture_rgb10_a2ui := False;
  GL_ARB_texture_storage_multisample := False;
  GL_ARB_texture_swizzle := False;
  GL_ARB_texture_view := False;
  GL_ARB_timer_query := False;
  GL_ARB_transform_feedback2 := False;
  GL_ARB_transform_feedback3 := False;
  GL_ARB_transpose_matrix := False;
  GL_ARB_uniform_buffer_object := False;
  GL_ARB_vertex_array_bgra := False;
  GL_ARB_vertex_array_object := False;
  GL_ARB_vertex_attrib_binding := False;
  GL_ARB_vertex_blend := False;
  GL_ARB_vertex_buffer_object := False;
  GL_ARB_vertex_program := False;
  GL_ARB_vertex_shader := False;
  GL_ARB_vertex_type_2_10_10_10_rev := False;
  GL_ARB_window_pos := False;
  GL_ATI_draw_buffers := False;
  GL_ATI_meminfo := False;
  GL_ATI_texture_compression_3dc := False;
  GL_ATI_texture_float := False;
  GL_ATI_texture_mirror_once := False;
  GL_EXT_abgr := False;
  GL_EXT_bgra := False;
  GL_EXT_bindable_uniform := False;
  GL_EXT_blend_color := False;
  GL_EXT_blend_equation_separate := False;
  GL_EXT_blend_func_separate := False;
  GL_EXT_blend_logic_op := False;
  GL_EXT_blend_minmax := False;
  GL_EXT_blend_subtract := False;
  GL_EXT_Cg_shader := False;
  GL_EXT_clip_volume_hint := False;
  GL_EXT_compiled_vertex_array := False;
  GL_EXT_copy_texture := False;
  GL_EXT_depth_bounds_test := False;
  GL_EXT_direct_state_access := False;
  GL_EXT_draw_buffers2 := False;
  GL_EXT_draw_instanced := False;
  GL_EXT_draw_range_elements := False;
  GL_EXT_fog_coord := False;
  GL_EXT_framebuffer_blit := False;
  GL_EXT_framebuffer_multisample := False;
  GL_EXT_framebuffer_object := False;
  GL_EXT_framebuffer_sRGB := False;
  GL_EXT_geometry_shader4 := False;
  GL_EXT_gpu_program_parameters := False;
  GL_EXT_gpu_shader4 := False;
  GL_EXT_multi_draw_arrays := False;
  GL_EXT_multisample := False;
  GL_EXT_packed_depth_stencil := False;
  GL_EXT_packed_float := False;
  GL_EXT_packed_pixels := False;
  GL_EXT_paletted_texture := False;
  GL_EXT_pixel_buffer_object := False;
  GL_EXT_polygon_offset := False;
  GL_EXT_rescale_normal := False;
  GL_EXT_secondary_color := False;
  GL_EXT_separate_specular_color := False;
  GL_EXT_shadow_funcs := False;
  GL_EXT_shared_texture_palette := False;
  GL_EXT_stencil_clear_tag := False;
  GL_EXT_stencil_two_side := False;
  GL_EXT_stencil_wrap := False;
  GL_EXT_texture_array := False;
  GL_EXT_texture_buffer_object := False;
  GL_EXT_texture_compression_latc := False;
  GL_EXT_texture_compression_rgtc := False;
  GL_EXT_texture_compression_s3tc := False;
  GL_EXT_texture_cube_map := False;
  GL_EXT_texture_edge_clamp := False;
  GL_EXT_texture_env_add := False;
  GL_EXT_texture_env_combine := False;
  GL_EXT_texture_env_dot3 := False;
  GL_EXT_texture_filter_anisotropic := False;
  GL_EXT_texture_integer := False;
  GL_EXT_texture_lod := False;
  GL_EXT_texture_lod_bias := False;
  GL_EXT_texture_mirror_clamp := False;
  GL_EXT_texture_object := False;
  GL_EXT_texture_rectangle := False;
  GL_EXT_texture_shared_exponent := False;
  GL_EXT_texture_sRGB := False;
  GL_EXT_texture_sRGB_decode := False;
  GL_EXT_texture_swizzle := False;
  GL_EXT_texture3D := False;
  GL_EXT_timer_query := False;
  GL_EXT_transform_feedback := False;
  GL_EXT_vertex_array := False;
  GL_GREMEDY_frame_terminator := False;
  GL_GREMEDY_string_marker := False;
  GL_HP_occlusion_test := False;
  GL_IBM_rasterpos_clip := False;
  GL_KHR_debug := False;
  GL_KTX_buffer_region := False;
  GL_MESA_resize_buffers := False;
  GL_NV_blend_square := False;
  GL_NV_conditional_render := False;
  GL_NV_copy_image := False;
  GL_NV_depth_buffer_float := False;
  GL_NV_fence := False;
  GL_NV_float_buffer := False;
  GL_NV_fog_distance := False;
  GL_NV_geometry_program4 := False;
  GL_NV_light_max_exponent := False;
  GL_NV_multisample_filter_hint := False;
  GL_NV_occlusion_query := False;
  GL_NV_path_rendering := False;
  GL_NV_point_sprite := False;
  GL_NV_primitive_restart := False;
  GL_NV_register_combiners := False;
  GL_NV_shader_buffer_load := False;
  GL_NV_texgen_reflection := False;
  GL_NV_texture_compression_vtc := False;
  GL_NV_texture_env_combine4 := False;
  GL_NV_texture_rectangle := False;
  GL_NV_texture_shader := False;
  GL_NV_texture_shader2 := False;
  GL_NV_texture_shader3 := False;
  GL_NV_transform_feedback := False;
  GL_NV_vdpau_interop := False;
  GL_NV_vertex_array_range := False;
  GL_NV_vertex_array_range2 := False;
  GL_NV_vertex_buffer_unified_memory := False;
  GL_NV_vertex_program := False;
  GL_NVX_gpu_memory_info := False;
  GL_S3_s3tc := False;
  GL_SGI_color_matrix := False;
  GL_SGIS_generate_mipmap := False;
  GL_SGIS_multisample := False;
  GL_SGIS_texture_border_clamp := False;
  GL_SGIS_texture_color_mask := False;
  GL_SGIS_texture_edge_clamp := False;
  GL_SGIS_texture_lod := False;
  GL_SGIX_depth_texture := False;
  GL_SGIX_shadow := False;
  GL_SGIX_shadow_ambient := False;

  FInitialized := False;
end;

{$IFDEF SUPPORT_WGL}
// ReadWGLImplementationProperties


procedure TVKExtensionsAndEntryPoints.ReadWGLImplementationProperties;
begin
  // ARB wgl extensions
  if Assigned(wglGetExtensionsStringARB) then
    FBuffer := string(wglGetExtensionsStringARB(wglGetCurrentDC))
  else
    FBuffer := '';
  WGL_ARB_buffer_region := CheckExtension('WGL_ARB_buffer_region');
  WGL_ARB_create_context := CheckExtension('WGL_ARB_create_context');
  WGL_ARB_create_context_profile := CheckExtension('WGL_ARB_create_context_profile');
  WGL_ARB_extensions_string := CheckExtension('WGL_ARB_extensions_string');
  WGL_ARB_framebuffer_sRGB := CheckExtension('WGL_ARB_framebuffer_sRGB');
  WGL_ARB_make_current_read := CheckExtension('WGL_ARB_make_current_read');
  WGL_ARB_multisample := CheckExtension('WGL_ARB_multisample');
  WGL_ARB_pbuffer := CheckExtension('WGL_ARB_pbuffer');
  WGL_ARB_pixel_format := CheckExtension('WGL_ARB_pixel_format');
  WGL_ARB_pixel_format_float := CheckExtension('WGL_ARB_pixel_format_float');
  WGL_ARB_render_texture := CheckExtension('WGL_ARB_render_texture');
  // Vendor/EXT wgl extensions
  WGL_ATI_pixel_format_float := CheckExtension('WGL_ATI_pixel_format_float');
  WGL_EXT_framebuffer_sRGB := CheckExtension('WGL_EXT_framebuffer_sRGB');
  WGL_EXT_pixel_format_packed_float := CheckExtension('WGL_EXT_pixel_format_packed_float');
  WGL_EXT_swap_control := CheckExtension('WGL_EXT_swap_control');
  WGL_NV_gpu_affinity := CheckExtension('WGL_NV_gpu_affinity');
  WGL_NV_DX_interop := CheckExtension('WGL_NV_DX_interop');
  WGL_NV_DX_interop2 := CheckExtension('WGL_NV_DX_interop2');
  WGL_EXT_create_context_es2_profile := CheckExtension('WGL_EXT_create_context_es2_profile');
end;

// ReadWGLExtensions


procedure TVKExtensionsAndEntryPoints.ReadWGLExtensions;
begin
  // ARB wgl extensions

  // ###########################################################
  // locating functions and procedures for
  // ARB approved WGL extensions
  // ###########################################################

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

  // ###########################################################
  // locating functions and procedures for
  // Vendor/EXT WGL extensions
  // ###########################################################

  // WGL_EXT_swap_control (EXT #172)
  wglSwapIntervalEXT := GLGetProcAddress('wglSwapIntervalEXT');
  wglGetSwapIntervalEXT := GLGetProcAddress('wglGetSwapIntervalEXT');

  // GL_NV_vertex_array_range (EXT #190)
  wglAllocateMemoryNV := GLGetProcAddress('wglAllocateMemoryNV');
  wglFreeMemoryNV := GLGetProcAddress('wglFreeMemoryNV');

  // WGL_NV_gpu_affinity
  wglEnumGpusNV := GLGetProcAddress('wglEnumGpusNV');
  wglEnumGpuDevicesNV := GLGetProcAddress('wglEnumGpuDevicesNV');
  wglCreateAffinityDCNV := GLGetProcAddress('wglCreateAffinityDCNV');
  wglEnumGpusFromAffinityDCNV := GLGetProcAddress('wglEnumGpusFromAffinityDCNV');
  wglDeleteDCNV := GLGetProcAddress('wglDeleteDCNV');

  // WGL_NV_DX_interop
  wglDXSetResourceShareHandleNV := GLGetProcAddress('wglDXSetResourceShareHandleNV');
  wglDXOpenDeviceNV := GLGetProcAddress('wglDXOpenDeviceNV');
  wglDXCloseDeviceNV := GLGetProcAddress('wglDXCloseDeviceNV');
  wglDXRegisterObjectNV := GLGetProcAddress('wglDXRegisterObjectNV');
  wglDXUnregisterObjectNV := GLGetProcAddress('wglDXUnregisterObjectNV');
  wglDXObjectAccessNV := GLGetProcAddress('wglDXObjectAccessNV');
  wglDXLockObjectsNV := GLGetProcAddress('wglDXLockObjectsNV');
  wglDXUnlockObjectsNV := GLGetProcAddress('wglDXUnlockObjectsNV');
end;

{$ENDIF}
{$IFDEF SUPPORT_GLX}
// ReadGLXImplementationProperties


procedure TVKExtensionsAndEntryPoints.ReadGLXImplementationProperties;
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


procedure TVKExtensionsAndEntryPoints.ReadGLXExtensions;
begin
  // ARB glx extensions

  // ###########################################################
  // locating functions and procedures for
  // ARB approved GLX extensions
  // ###########################################################

  // GLX 1.3 and later
  XChooseFBConfig := GLGetProcAddress('glXChooseFBConfig');
  XGetFBConfigAttrib := GLGetProcAddress('glXGetFBConfigAttrib');
  XGetFBConfigs := GLGetProcAddress('glXGetFBConfigs');
  XGetVisualFromFBConfig := GLGetProcAddress('glXGetVisualFromFBConfig');
  XCreateWindow := GLGetProcAddress('glXCreateWindow');
  XDestroyWindow := GLGetProcAddress('glXDestroyWindow');
  XCreatePixmap := GLGetProcAddress('glXCreatePixmap');
  XDestroyPixmap := GLGetProcAddress('glXDestroyPixmap');
  XCreatePbuffer := GLGetProcAddress('glXCreatePbuffer');
  XDestroyPbuffer := GLGetProcAddress('glXDestroyPbuffer');
  XQueryDrawable := GLGetProcAddress('glXQueryDrawable');
  XCreateNewContext := GLGetProcAddress('glXCreateNewContext');
  XMakeContextCurrent := GLGetProcAddress('glXMakeContextCurrent');
  XGetCurrentReadDrawable := GLGetProcAddress('glXGetCurrentReadDrawable');
  XQueryContext := GLGetProcAddress('glXQueryContext');
  XSelectEvent := GLGetProcAddress('glXSelectEvent');
  XGetSelectedEvent := GLGetProcAddress('glXGetSelectedEvent');
  XBindTexImageARB := GLGetProcAddress('glXBindTexImageARB');
  XReleaseTexImageARB := GLGetProcAddress('glXReleaseTexImageARB');
  XDrawableAttribARB := GLGetProcAddress('glxDrawableAttribARB');

  // GLX 1.4
  // GLX_ARB_create_context (EXT #56)
  XCreateContextAttribsARB := GLGetProcAddress('glXCreateContextAttribsARB');

  // ###########################################################
  // locating functions and procedures for
  // Vendor/EXT WGL extensions
  // ###########################################################

  // WGL_EXT_swap_control (EXT #172)
  XSwapIntervalSGI := GLGetProcAddress('glXSwapIntervalSGI');
  XGetVideoSyncSGI := GLGetProcAddress('glXGetVideoSyncSGI');
  XWaitVideoSyncSGI := GLGetProcAddress('glXWaitVideoSyncSGI');
  XFreeContextEXT := GLGetProcAddress('glXFreeContextEXT');
  XGetContextIDEXT := GLGetProcAddress('glXGetContextIDEXT');
  XGetCurrentDisplayEXT := GLGetProcAddress('glXGetCurrentDisplayEXT');
  XImportContextEXT := GLGetProcAddress('glXImportContextEXT');
  XQueryContextInfoEXT := GLGetProcAddress('glXQueryContextInfoEXT');
  XCopySubBufferMESA := GLGetProcAddress('glXCopySubBufferMESA');
  XCreateGLXPixmapMESA := GLGetProcAddress('glXCreateGLXPixmapMESA');
  XReleaseBuffersMESA := GLGetProcAddress('glXReleaseBuffersMESA');
  XSet3DfxModeMESA := GLGetProcAddress('glXSet3DfxModeMESA');

  XBindTexImageEXT := GLGetProcAddress('glXBindTexImageEXT');
  XReleaseTexImageEXT := GLGetProcAddress('glXReleaseTexImageEXT');

  // GLX 1.4
  XMakeCurrentReadSGI := GLGetProcAddress('glXMakeCurrentReadSGI');
  XGetCurrentReadDrawableSGI := GLGetProcAddress('glXGetCurrentReadDrawableSGI');
  XGetFBConfigAttribSGIX := GLGetProcAddress('glXGetFBConfigAttribSGIX');
  XChooseFBConfigSGIX := GLGetProcAddress('glXChooseFBConfigSGIX');
  XCreateGLXPixmapWithConfigSGIX := GLGetProcAddress('glXCreateGLXPixmapWithConfigSGIX');
  XCreateContextWithConfigSGIX := GLGetProcAddress('glXCreateContextWithConfigSGIX');
  XGetVisualFromFBConfigSGIX := GLGetProcAddress('glXGetVisualFromFBConfigSGIX');
  XGetFBConfigFromVisualSGIX := GLGetProcAddress('glXGetFBConfigFromVisualSGIX');
  XCreateGLXPbufferSGIX := GLGetProcAddress('glXCreateGLXPbufferSGIX');
  XDestroyGLXPbufferSGIX := GLGetProcAddress('glXDestroyGLXPbufferSGIX');
  XQueryGLXPbufferSGIX := GLGetProcAddress('glXQueryGLXPbufferSGIX');
  XSelectEventSGIX := GLGetProcAddress('glXSelectEventSGIX');
  XGetSelectedEventSGIX := GLGetProcAddress('glXGetSelectedEventSGIX');
  XCushionSGI := GLGetProcAddress('glXCushionSGI');
  XBindChannelToWindowSGIX := GLGetProcAddress('glXBindChannelToWindowSGIX');
  XChannelRectSGIX := GLGetProcAddress('glXChannelRectSGIX');
  XQueryChannelRectSGIX := GLGetProcAddress('glXQueryChannelRectSGIX');
  XQueryChannelDeltasSGIX := GLGetProcAddress('glXQueryChannelDeltasSGIX');
  XChannelRectSyncSGIX := GLGetProcAddress('glXChannelRectSyncSGIX');
  XJoinSwapGroupSGIX := GLGetProcAddress('glXJoinSwapGroupSGIX');
  XBindSwapBarrierSGIX := GLGetProcAddress('glXBindSwapBarrierSGIX');
  XQueryMaxSwapBarriersSGIX := GLGetProcAddress('glXQueryMaxSwapBarriersSGIX');
  XQueryHyperpipeNetworkSGIX := GLGetProcAddress('glXQueryHyperpipeNetworkSGIX');

  XHyperpipeConfigSGIX := GLGetProcAddress('glXHyperpipeConfigSGIX');
  XQueryHyperpipeConfigSGIX := GLGetProcAddress('glXQueryHyperpipeConfigSGIX');
  XDestroyHyperpipeConfigSGIX := GLGetProcAddress('glXDestroyHyperpipeConfigSGIX');
  XBindHyperpipeSGIX := GLGetProcAddress('glXBindHyperpipeSGIX');
  XQueryHyperpipeBestAttribSGIX := GLGetProcAddress('glXQueryHyperpipeBestAttribSGIX');
  XHyperpipeAttribSGIX := GLGetProcAddress('glXHyperpipeAttribSGIX');
  XQueryHyperpipeAttribSGIX := GLGetProcAddress('glXQueryHyperpipeAttribSGIX');
  XGetAGPOffsetMESA := GLGetProcAddress('glXGetAGPOffsetMESA');
  XEnumerateVideoDevicesNV := GLGetProcAddress('glXEnumerateVideoDevicesNV');
  XBindVideoDeviceNV := GLGetProcAddress('glXBindVideoDeviceNV');
  XGetVideoDeviceNV := GLGetProcAddress('glXGetVideoDeviceNV');
  XCopySubBufferMESA := GLGetProcAddress('glXCopySubBufferMESA');
  XReleaseBuffersMESA := GLGetProcAddress('glXReleaseBuffersMESA');
  XCreateGLXPixmapMESA := GLGetProcAddress('glXCreateGLXPixmapMESA');
  XSet3DfxModeMESA := GLGetProcAddress('glXSet3DfxModeMESA');

  XAllocateMemoryNV := GLGetProcAddress('glXAllocateMemoryNV');
  XFreeMemoryNV := GLGetProcAddress('glXFreeMemoryNV');

  XReleaseVideoDeviceNV := GLGetProcAddress('glXReleaseVideoDeviceNV');
  XBindVideoImageNV := GLGetProcAddress('glXBindVideoImageNV');
  XReleaseVideoImageNV := GLGetProcAddress('glXReleaseVideoImageNV');
  XSendPbufferToVideoNV := GLGetProcAddress('glXSendPbufferToVideoNV');
  XGetVideoInfoNV := GLGetProcAddress('glXGetVideoInfoNV');
  XJoinSwapGroupNV := GLGetProcAddress('glXJoinSwapGroupNV');
  XBindSwapBarrierNV := GLGetProcAddress('glXBindSwapBarrierNV');
  XQuerySwapGroupNV := GLGetProcAddress('glXQuerySwapGroupNV');
  XQueryMaxSwapGroupsNV := GLGetProcAddress('glXQueryMaxSwapGroupsNV');
  XQueryFrameCountNV := GLGetProcAddress('glXQueryFrameCountNV');
  XResetFrameCountNV := GLGetProcAddress('glXResetFrameCountNV');
  XBindVideoCaptureDeviceNV := GLGetProcAddress('glXBindVideoCaptureDeviceNV');
  XEnumerateVideoCaptureDevicesNV :=
    GLGetProcAddress('glXEnumerateVideoCaptureDevicesNV');
  XLockVideoCaptureDeviceNV := GLGetProcAddress('glxLockVideoCaptureDeviceNV');
  XQueryVideoCaptureDeviceNV := GLGetProcAddress('glXQueryVideoCaptureDeviceNV');
  XReleaseVideoCaptureDeviceNV := GLGetProcAddress('glXReleaseVideoCaptureDeviceNV');
  XSwapIntervalEXT := GLGetProcAddress('glXSwapIntervalEXT');
  XCopyImageSubDataNV := GLGetProcAddress('glXCopyImageSubDataNV');
end;

{$ENDIF}

{$IFDEF DARWIN}
// ReadAGLImplementationProperties


procedure TVKExtensionsAndEntryPoints.ReadAGLImplementationProperties;
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

procedure TVKExtensionsAndEntryPoints.ReadAGLExtensions;
begin
  // Managing pixel format object
  ACreatePixelFormat := AGLGetProcAddress('aglCreatePixelFormat');
  AChoosePixelFormat := AGLGetProcAddress('aglChoosePixelFormat');
  ADestroyPixelFormat := AGLGetProcAddress('aglDestroyPixelFormat');
  ADescribePixelFormat := AGLGetProcAddress('aglDescribePixelFormat');
  ADestroyPixelFormat := AGLGetProcAddress('aglDestroyPixelFormat');
  AGetCGLPixelFormat := AGLGetProcAddress('aglGetCGLPixelFormat');
  ADisplaysOfPixelFormat := AGLGetProcAddress('aglDisplaysOfPixelFormat');
  ANextPixelFormat := AGLGetProcAddress('aglNextPixelFormat');
  // Managing context
  ACreateContext := AGLGetProcAddress('aglCreateContext');
  ACopyContext := AGLGetProcAddress('aglCopyContext');
  ADestroyContext := AGLGetProcAddress('aglDestroyContext');
  AUpdateContext := AGLGetProcAddress('aglUpdateContext');
  ASetCurrentContext := AGLGetProcAddress('aglSetCurrentContext');
  AGetCGLContext := AGLGetProcAddress('aglGetCGLContext');
  AGetCurrentContext := AGLGetProcAddress('aglGetCurrentContext');
  ASwapBuffers := AGLGetProcAddress('aglSwapBuffers');
  AUpdateContext := AGLGetProcAddress('aglUpdateContext');
  // Managing Pixel Buffers
  ACreatePBuffer := AGLGetProcAddress('aglCreatePBuffer');
  ADestroyPBuffer := AGLGetProcAddress('aglDestroyPBuffer');
  ADescribePBuffer := AGLGetProcAddress('aglDescribePBuffer');
  AGetPBuffer := AGLGetProcAddress('aglGetPBuffer');
  ASetPBuffer := AGLGetProcAddress('aglSetPBuffer');
  ATexImagePBuffer := AGLGetProcAddress('aglTexImagePBuffer');
  // Managing Drawable Objects
  ASetDrawable := AGLGetProcAddress('aglSetDrawable'); // deprecated
  AGetDrawable := AGLGetProcAddress('aglGetDrawable'); // deprecated
  ASetFullScreen := AGLGetProcAddress('aglSetFullScreen');
  ASetOffScreen := AGLGetProcAddress('aglSetOffScreen');
  // Getting and Setting Context Options
  AEnable := AGLGetProcAddress('aglEnable');
  ADisable := AGLGetProcAddress('aglDisable');
  AIsEnabled := AGLGetProcAddress('aglIsEnabled');
  ASetInteger := AGLGetProcAddress('aglSetInteger');
  AGetInteger := AGLGetProcAddress('aglGetInteger');
  // Getting and Setting Global Information
  AConfigure := AGLGetProcAddress('aglConfigure');
  AGetVersion := AGLGetProcAddress('aglGetVersion');
  AResetLibrary := AGLGetProcAddress('aglResetLibrary');
  // Getting Renderer Information
  ADescribeRenderer := AGLGetProcAddress('aglDescribeRenderer');
  ADestroyRendererInfo := AGLGetProcAddress('aglDestroyRendererInfo');
  ANextRendererInfo := AGLGetProcAddress('aglNextRendererInfo');
  AQueryRendererInfoForCGDirectDisplayIDs := AGLGetProcAddress('aglQueryRendererInfoForCGDirectDisplayIDs');
  // Managing Virtual Screens
  AGetVirtualScreen := AGLGetProcAddress('aglGetVirtualScreen');
  ASetVirtualScreen := AGLGetProcAddress('aglSetVirtualScreen');
  // Getting and Setting Windows
  ASetWindowRef := AGLGetProcAddress('aglSetWindowRef');
  AGetWindowRef := AGLGetProcAddress('aglGetWindowRef');
  // Getting and Setting HIView Objects
  ASetHIViewRef := AGLGetProcAddress('aglSetHIViewRef');
  AGetHIViewRef := AGLGetProcAddress('aglGetHIViewRef');
  // Getting Error Information
  AGetError := AGLGetProcAddress('aglGetError');
  AErrorString := AGLGetProcAddress('aglErrorString');
end;
{$ENDIF}

{$IFDEF EGL_SUPPORT}
procedure TVKExtensionsAndEntryPoints.ReadEGLImplementationProperties;
var
  MajorVersion, MinorVersion: integer;
begin
  if Assigned(GetString) then
    FBuffer := string(GetString(GL_EXTENSIONS))
  else
    FBuffer := '';

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
end;

procedure TVKExtensionsAndEntryPoints.ReadEGLExtensions;
begin
  EGetError := EGLGetProcAddress('eglGetError');
  EGetDisplay := EGLGetProcAddress('eglGetDisplay');
  EInitialize := EGLGetProcAddress('eglInitialize');
  ETerminate := EGLGetProcAddress('eglTerminate');
  EQueryString := EGLGetProcAddress('eglQueryString');
  EGetConfigs := EGLGetProcAddress('eglGetConfigs');
  EChooseConfig := EGLGetProcAddress('eglChooseConfig');
  EGetConfigAttrib := EGLGetProcAddress('eglGetConfigAttrib');
  ECreatePixmapSurface := EGLGetProcAddress('eglCreatePixmapSurface');
  EDestroySurface := EGLGetProcAddress('eglDestroySurface');
  EQuerySurface := EGLGetProcAddress('eglQuerySurface');
  EBindAPI := EGLGetProcAddress('eglBindAPI');
  EQueryAPI := EGLGetProcAddress('eglQueryAPI');
  EWaitClient := EGLGetProcAddress('eglWaitClient');
  EReleaseThread := EGLGetProcAddress('eglReleaseThread');
  ECreatePbufferFromClientBuffer := EGLGetProcAddress('eglCreatePbufferFromClientBuffer');
  ESurfaceAttrib := EGLGetProcAddress('eglSurfaceAttrib');
  EBindTexImage := EGLGetProcAddress('eglBindTexImage');
  EReleaseTexImage := EGLGetProcAddress('eglReleaseTexImage');
  ESwapInterval := EGLGetProcAddress('eglSwapInterval');
  ECreateContext := EGLGetProcAddress('eglCreateContext');
  EDestroyContext := EGLGetProcAddress('eglDestroyContext');
  EMakeCurrent := EGLGetProcAddress('eglMakeCurrent');
  EGetCurrentContext := EGLGetProcAddress('eglGetCurrentContext');
  EGetCurrentSurface := EGLGetProcAddress('eglGetCurrentSurface');
  EGetCurrentDisplay := EGLGetProcAddress('eglGetCurrentDisplay');
  EQueryContext := EGLGetProcAddress('eglQueryContext');
  EWaitGL := EGLGetProcAddress('eglWaitGL');
  EWaitNative := EGLGetProcAddress('eglWaitNative');
  ESwapBuffers := EGLGetProcAddress('eglSwapBuffers');
  ECopyBuffers := EGLGetProcAddress('eglCopyBuffers');
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
    if (Separator > 1) and (Separator < Length(Buffer)) and 
	  (AnsiChar(Buffer[Separator - 1]) in ['0'..'9']) and
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
      while (Separator <= Length(Buffer)) and 
	    (AnsiChar(Buffer[Separator]) in ['0'..'9']) do
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

function IsVersionMet(MajorVersion, MinorVersion, actualMajorVersion,
  actualMinorVersion: integer): boolean;
begin
  Result := (actualMajorVersion > MajorVersion) or
    ((actualMajorVersion = MajorVersion) and (actualMinorVersion >= MinorVersion));
end;

// InitOpenGL
//
function InitOpenGL : Boolean;
begin
{$IFNDEF EGL_SUPPORT}
  if (GLHandle = INVALID_MODULEHANDLE) or (GLUHandle = INVALID_MODULEHANDLE) then
    Result := InitOpenGLFromLibrary(opengl32, glu32)
  else
    Result := True;
{$ELSE}
  CloseOpenGL;
  Result := True;
{$IFNDEF DARWIN}
  EGLHandle := LoadLibrary(PChar(libEGL));
  Result := EGLHandle <> INVALID_MODULEHANDLE;
{$ENDIF}
  EGL2Handle := LoadLibrary(PChar(libGLES2));
  Result := Result and (EGL2Handle <> INVALID_MODULEHANDLE);
{$ENDIF}
end;

// InitOpenGLFromLibrary
//
function InitOpenGLFromLibrary(const GLName, GLUName : String) : Boolean;
begin
  Result := False;
  CloseOpenGL;

   GLHandle:=LoadLibrary(PChar(GLName));
   GLUHandle:=LoadLibrary(PChar(GLUName));

   {$IFDEF Linux}   // make it work when mesa-dev is not installed and only libGL.so.1 is available
   if (GLHandle=INVALID_MODULEHANDLE) then
         GLHandle:=LoadLibrary(PChar(GLName+'.1'));
  if (GLUHandle=INVALID_MODULEHANDLE) then
         GLUHandle:=LoadLibrary(PChar(GLUName+'.1'));
{$ENDIF}
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
//
function IsOpenGLInitialized: Boolean;
begin
  Result :={$IFNDEF EGL_SUPPORT}(GLHandle <> INVALID_MODULEHANDLE){$ELSE}(EGL2Handle <> INVALID_MODULEHANDLE){$ENDIF};
end;

// CloseOpenGL
//

procedure CloseOpenGL;
begin
{$IFNDEF EGL_SUPPORT}
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
  if EGL2Handle <> INVALID_MODULEHANDLE then
  begin
    FreeLibrary(EGL2Handle);
    EGL2Handle := INVALID_MODULEHANDLE;
  end;
{$ENDIF}
end;

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
  Result := GLGetProcAddress('glResizeBuffersMESA') <> nil;
end;

initialization

  Set8087CW($133F);

finalization

  CloseOpenGL;

end.