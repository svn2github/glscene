//
// VXScene Component Library, based on GLScene http://glscene.sourceforge.net
//
{
  OpenGL adapter
}
unit VXS.OpenGLAdapter;

interface

{$I VXScene.inc}

uses
  Winapi.Windows,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  FMX.Dialogs,
  
  VXS.Strings,
  VXS.VectorGeometry,
  VXS.VectorTypes;

type
  PGLChar = PAnsiChar;
  TGLString = AnsiString;

  PGPUDEVICE = ^TGPUDEVICE;
  TGPUDEVICE = record
    cb: Cardinal;
    DeviceName: array [0 .. 31] of AnsiChar;
    DeviceString: array [0 .. 127] of AnsiChar;
    Flags: Cardinal;
    rcVirtualScreen: TRect;
  end;

  // WGL_ARB_pbuffer
  HPBUFFERARB = THandle;

  // WGL_EXT_pbuffer
  HPBUFFEREXT = THandle;

  // WGL_NV_present_video
  PHVIDEOOUTPUTDEVICENV = ^HVIDEOOUTPUTDEVICENV;
  HVIDEOOUTPUTDEVICENV = THandle;

   // WGL_NV_video_output
  PHPVIDEODEV = ^HPVIDEODEV;
  HPVIDEODEV = THandle;

   // WGL_NV_gpu_affinity
  PHPGPUNV = ^HPGPUNV;
  PHGPUNV = ^HGPUNV;

   // WGL_NV_video_capture
  HVIDEOINPUTDEVICENV = THandle;
  PHVIDEOINPUTDEVICENV = ^HVIDEOINPUTDEVICENV;

  HPGPUNV = THandle;
  HGPUNV = THandle;

type
   // GLU types
   TGLUNurbs = record
   end;
   TGLUQuadric = record
   end;
   TGLUTesselator = record
   end;
   PGLUNurbs = ^TGLUNurbs;
   PGLUQuadric = ^TGLUQuadric;
   PGLUTesselator=  ^TGLUTesselator;
   // backwards compatibility
   TGLUNurbsObj = TGLUNurbs;
   TGLUQuadricObj = TGLUQuadric;
   TGLUTesselatorObj = TGLUTesselator;
   TGLUTriangulatorObj = TGLUTesselator;
   PGLUNurbsObj = PGLUNurbs;
   PGLUQuadricObj = PGLUQuadric;
   PGLUTesselatorObj = PGLUTesselator;
   PGLUTriangulatorObj = PGLUTesselator;

var
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
  GLU_VERSION_1_3,
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
  WIN_swap_hint: Boolean;

const
  // GL_EXT_framebuffer_object not found in Winapi.OpenGLext
  GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT = $8CD8;

  // WGL_3DFX_multisample
  WGL_SAMPLE_BUFFERS_3DFX = $2060;
  WGL_SAMPLES_3DFX = $2061;

  // WGL_ARB_buffer_region
  WGL_FRONT_COLOR_BUFFER_BIT_ARB = $00000001;
  WGL_BACK_COLOR_BUFFER_BIT_ARB = $00000002;
  WGL_DEPTH_BUFFER_BIT_ARB = $00000004;
  WGL_STENCIL_BUFFER_BIT_ARB = $00000008;

  // WGL_ARB_context_flush_control
  WGL_CONTEXT_RELEASE_BEHAVIOR_ARB = $2097;
  WGL_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB = 0;
  WGL_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB = $2098;

  // WGL_ARB_make_current_read
  ERROR_INVALID_PIXEL_TYPE_ARB = $2043;
  ERROR_INCOMPATIBLE_DEVICE_CONTEXTS_ARB = $2054;

  // WGL_ARB_multisample
  WGL_SAMPLE_BUFFERS_ARB = $2041;
  WGL_SAMPLES_ARB = $2042;

  // WGL_ARB_pbuffer
  WGL_DRAW_TO_PBUFFER_ARB = $202D;
  WGL_MAX_PBUFFER_PIXELS_ARB = $202E;
  WGL_MAX_PBUFFER_WIDTH_ARB = $202F;
  WGL_MAX_PBUFFER_HEIGHT_ARB = $2030;
  WGL_PBUFFER_LARGEST_ARB = $2033;
  WGL_PBUFFER_WIDTH_ARB = $2034;
  WGL_PBUFFER_HEIGHT_ARB = $2035;
  WGL_PBUFFER_LOST_ARB = $2036;

  // WGL_ARB_pixel_format
  WGL_NUMBER_PIXEL_FORMATS_ARB = $2000;
  WGL_DRAW_TO_WINDOW_ARB = $2001;
  WGL_DRAW_TO_BITMAP_ARB = $2002;
  WGL_ACCELERATION_ARB = $2003;
  WGL_NEED_PALETTE_ARB = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB = $2006;
  WGL_SWAP_METHOD_ARB = $2007;
  WGL_NUMBER_OVERLAYS_ARB = $2008;
  WGL_NUMBER_UNDERLAYS_ARB = $2009;
  WGL_TRANSPARENT_ARB = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB = $203B;
  WGL_SHARE_DEPTH_ARB = $200C;
  WGL_SHARE_STENCIL_ARB = $200D;
  WGL_SHARE_ACCUM_ARB = $200E;
  WGL_SUPPORT_GDI_ARB = $200F;
  WGL_SUPPORT_OPENGL_ARB = $2010;
  WGL_DOUBLE_BUFFER_ARB = $2011;
  WGL_STEREO_ARB = $2012;
  WGL_PIXEL_TYPE_ARB = $2013;
  WGL_COLOR_BITS_ARB = $2014;
  WGL_RED_BITS_ARB = $2015;
  WGL_RED_SHIFT_ARB = $2016;
  WGL_GREEN_BITS_ARB = $2017;
  WGL_GREEN_SHIFT_ARB = $2018;
  WGL_BLUE_BITS_ARB = $2019;
  WGL_BLUE_SHIFT_ARB = $201A;
  WGL_ALPHA_BITS_ARB = $201B;
  WGL_ALPHA_SHIFT_ARB = $201C;
  WGL_ACCUM_BITS_ARB = $201D;
  WGL_ACCUM_RED_BITS_ARB = $201E;
  WGL_ACCUM_GREEN_BITS_ARB = $201F;
  WGL_ACCUM_BLUE_BITS_ARB = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB = $2021;
  WGL_DEPTH_BITS_ARB = $2022;
  WGL_STENCIL_BITS_ARB = $2023;
  WGL_AUX_BUFFERS_ARB = $2024;
  WGL_NO_ACCELERATION_ARB = $2025;
  WGL_GENERIC_ACCELERATION_ARB = $2026;
  WGL_FULL_ACCELERATION_ARB = $2027;
  WGL_SWAP_EXCHANGE_ARB = $2028;
  WGL_SWAP_COPY_ARB = $2029;
  WGL_SWAP_UNDEFINED_ARB = $202A;
  WGL_TYPE_RGBA_ARB = $202B;
  WGL_TYPE_COLORINDEX_ARB = $202C;

  // WGL_ARB_pixel_format_float
  WGL_RGBA_FLOAT_MODE_ARB = $8820;
  WGL_CLAMP_VERTEX_COLOR_ARB = $891A;
  WGL_CLAMP_FRAGMENT_COLOR_ARB = $891B;
  WGL_CLAMP_READ_COLOR_ARB = $891C;
  WGL_FIXED_ONLY_ARB = $891D;

  // WGL_ARB_render_texture
  WGL_BIND_TO_TEXTURE_RGB_ARB = $2070;
  WGL_BIND_TO_TEXTURE_RGBA_ARB = $2071;
  WGL_TEXTURE_FORMAT_ARB = $2072;
  WGL_TEXTURE_TARGET_ARB = $2073;
  WGL_MIPMAP_TEXTURE_ARB = $2074;
  WGL_TEXTURE_RGB_ARB = $2075;
  WGL_TEXTURE_RGBA_ARB = $2076;
  WGL_NO_TEXTURE_ARB = $2077;
  WGL_TEXTURE_CUBE_MAP_ARB = $2078;
  WGL_TEXTURE_1D_ARB = $2079;
  WGL_TEXTURE_2D_ARB = $207A;
  WGL_MIPMAP_LEVEL_ARB = $207B;
  WGL_CUBE_MAP_FACE_ARB = $207C;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $207D;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $207E;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $207F;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $2080;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $2081;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $2082;
  WGL_FRONT_LEFT_ARB = $2083;
  WGL_FRONT_RIGHT_ARB = $2084;
  WGL_BACK_LEFT_ARB = $2085;
  WGL_BACK_RIGHT_ARB = $2086;
  WGL_AUX0_ARB = $2087;
  WGL_AUX1_ARB = $2088;
  WGL_AUX2_ARB = $2089;
  WGL_AUX3_ARB = $208A;
  WGL_AUX4_ARB = $208B;
  WGL_AUX5_ARB = $208C;
  WGL_AUX6_ARB = $208D;
  WGL_AUX7_ARB = $208E;
  WGL_AUX8_ARB = $208F;
  WGL_AUX9_ARB = $2090;

  // WGL_ARB_robustness_application_isolation
  WGL_CONTEXT_RESET_ISOLATION_BIT_ARB = $00000008;

  // WGL_ARB_create_context
  WGL_CONTEXT_DEBUG_BIT_ARB = $00000001;
  WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = $00000002;
  WGL_CONTEXT_MAJOR_VERSION_ARB = $2091;
  WGL_CONTEXT_MINOR_VERSION_ARB = $2092;
  WGL_CONTEXT_LAYER_PLANE_ARB = $2093;
  WGL_CONTEXT_FLAGS_ARB = $2094;
  ERROR_INVALID_VERSION_ARB = $2095;

  // WGL_ARB_create_context_profile
  WGL_CONTEXT_PROFILE_MASK_ARB = $9126;
  WGL_CONTEXT_CORE_PROFILE_BIT_ARB = $00000001;
  WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = $00000002;
  ERROR_INVALID_PROFILE_ARB = $2096;

  // WGL_ARB_framebuffer_sRGB
  WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB = $20A9;

  // WGL_ARB_create_context_robustness
  WGL_CONTEXT_ROBUST_ACCESS_BIT_ARB = $00000004;
  WGL_LOSE_CONTEXT_ON_RESET_ARB = $8252;
  WGL_CONTEXT_RESET_NOTIFICATION_STRATEGY_ARB = $8256;
  WGL_NO_RESET_NOTIFICATION_ARB = $8261;

  // WGL_ATI_pixel_format_float
  WGL_TYPE_RGBA_FLOAT_ATI = $21A0;
  GL_TYPE_RGBA_FLOAT_ATI = $8820;
  GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI = $8835;

  // WGL_AMD_gpu_association
  WGL_GPU_VENDOR_AMD = $1F00;
  WGL_GPU_RENDERER_STRING_AMD = $1F01;
  WGL_GPU_OPENGL_VERSION_STRING_AMD = $1F02;
  WGL_GPU_FASTEST_TARGET_GPUS_AMD = $21A2;
  WGL_GPU_RAM_AMD = $21A3;
  WGL_GPU_CLOCK_AMD = $21A4;
  WGL_GPU_NUM_PIPES_AMD = $21A5;
  WGL_GPU_NUM_SIMD_AMD = $21A6;
  WGL_GPU_NUM_RB_AMD = $21A7;
  WGL_GPU_NUM_SPI_AMD = $21A8;

  // WGL_EXT_depth_float
  WGL_DEPTH_FLOAT_EXT = $2040;

  // WGL_EXT_make_current_read
  ERROR_INVALID_PIXEL_TYPE_EXT = $2043;

  // WGL_EXT_multisample
  WGL_SAMPLE_BUFFERS_EXT = $2041;
  WGL_SAMPLES_EXT = $2042;

  // WGL_EXT_pbuffer
  WGL_DRAW_TO_PBUFFER_EXT = $202D;
  WGL_MAX_PBUFFER_PIXELS_EXT = $202E;
  WGL_MAX_PBUFFER_WIDTH_EXT = $202F;
  WGL_MAX_PBUFFER_HEIGHT_EXT = $2030;
  WGL_OPTIMAL_PBUFFER_WIDTH_EXT = $2031;
  WGL_OPTIMAL_PBUFFER_HEIGHT_EXT = $2032;
  WGL_PBUFFER_LARGEST_EXT = $2033;
  WGL_PBUFFER_WIDTH_EXT = $2034;
  WGL_PBUFFER_HEIGHT_EXT = $2035;

  // WGL_EXT_pixel_format
  WGL_NUMBER_PIXEL_FORMATS_EXT = $2000;
  WGL_DRAW_TO_WINDOW_EXT = $2001;
  WGL_DRAW_TO_BITMAP_EXT = $2002;
  WGL_ACCELERATION_EXT = $2003;
  WGL_NEED_PALETTE_EXT = $2004;
  WGL_NEED_SYSTEM_PALETTE_EXT = $2005;
  WGL_SWAP_LAYER_BUFFERS_EXT = $2006;
  WGL_SWAP_METHOD_EXT = $2007;
  WGL_NUMBER_OVERLAYS_EXT = $2008;
  WGL_NUMBER_UNDERLAYS_EXT = $2009;
  WGL_TRANSPARENT_EXT = $200A;
  WGL_TRANSPARENT_VALUE_EXT = $200B;
  WGL_SHARE_DEPTH_EXT = $200C;
  WGL_SHARE_STENCIL_EXT = $200D;
  WGL_SHARE_ACCUM_EXT = $200E;
  WGL_SUPPORT_GDI_EXT = $200F;
  WGL_SUPPORT_OPENGL_EXT = $2010;
  WGL_DOUBLE_BUFFER_EXT = $2011;
  WGL_STEREO_EXT = $2012;
  WGL_PIXEL_TYPE_EXT = $2013;
  WGL_COLOR_BITS_EXT = $2014;
  WGL_RED_BITS_EXT = $2015;
  WGL_RED_SHIFT_EXT = $2016;
  WGL_GREEN_BITS_EXT = $2017;
  WGL_GREEN_SHIFT_EXT = $2018;
  WGL_BLUE_BITS_EXT = $2019;
  WGL_BLUE_SHIFT_EXT = $201A;
  WGL_ALPHA_BITS_EXT = $201B;
  WGL_ALPHA_SHIFT_EXT = $201C;
  WGL_ACCUM_BITS_EXT = $201D;
  WGL_ACCUM_RED_BITS_EXT = $201E;
  WGL_ACCUM_GREEN_BITS_EXT = $201F;
  WGL_ACCUM_BLUE_BITS_EXT = $2020;
  WGL_ACCUM_ALPHA_BITS_EXT = $2021;
  WGL_DEPTH_BITS_EXT = $2022;
  WGL_STENCIL_BITS_EXT = $2023;
  WGL_AUX_BUFFERS_EXT = $2024;
  WGL_NO_ACCELERATION_EXT = $2025;
  WGL_GENERIC_ACCELERATION_EXT = $2026;
  WGL_FULL_ACCELERATION_EXT = $2027;
  WGL_SWAP_EXCHANGE_EXT = $2028;
  WGL_SWAP_COPY_EXT = $2029;
  WGL_SWAP_UNDEFINED_EXT = $202A;
  WGL_TYPE_RGBA_EXT = $202B;
  WGL_TYPE_COLORINDEX_EXT = $202C;

  // WGL_I3D_digital_video_control
  WGL_DIGITAL_VIDEO_CURSOR_ALPHA_FRAMEBUFFER_I3D = $2050;
  WGL_DIGITAL_VIDEO_CURSOR_ALPHA_VALUE_I3D = $2051;
  WGL_DIGITAL_VIDEO_CURSOR_INCLUDED_I3D = $2052;
  WGL_DIGITAL_VIDEO_GAMMA_CORRECTED_I3D = $2053;

  // WGL_I3D_gamma
  WGL_GAMMA_TABLE_SIZE_I3D = $204E;
  WGL_GAMMA_EXCLUDE_DESKTOP_I3D = $204F;

  // WGL_I3D_genlock
  WGL_GENLOCK_SOURCE_MULTIVIEW_I3D = $2044;
  WGL_GENLOCK_SOURCE_EXTENAL_SYNC_I3D = $2045;
  WGL_GENLOCK_SOURCE_EXTENAL_FIELD_I3D = $2046;
  WGL_GENLOCK_SOURCE_EXTENAL_TTL_I3D = $2047;
  WGL_GENLOCK_SOURCE_DIGITAL_SYNC_I3D = $2048;
  WGL_GENLOCK_SOURCE_DIGITAL_FIELD_I3D = $2049;
  WGL_GENLOCK_SOURCE_EDGE_FALLING_I3D = $204A;
  WGL_GENLOCK_SOURCE_EDGE_RISING_I3D = $204B;
  WGL_GENLOCK_SOURCE_EDGE_BOTH_I3D = $204C;

  // WGL_I3D_image_buffer
  WGL_IMAGE_BUFFER_MIN_ACCESS_I3D = $00000001;
  WGL_IMAGE_BUFFER_LOCK_I3D = $00000002;

  // WGL_NV_float_buffer
  WGL_FLOAT_COMPONENTS_NV = $20B0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV = $20B1;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV = $20B2;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV = $20B3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV = $20B4;
  WGL_TEXTURE_FLOAT_R_NV = $20B5;
  WGL_TEXTURE_FLOAT_RG_NV = $20B6;
  WGL_TEXTURE_FLOAT_RGB_NV = $20B7;
  WGL_TEXTURE_FLOAT_RGBA_NV = $20B8;

  // WGL_NV_render_depth_texture
  WGL_BIND_TO_TEXTURE_DEPTH_NV = $20A3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_DEPTH_NV = $20A4;
  WGL_DEPTH_TEXTURE_FORMAT_NV = $20A5;
  WGL_TEXTURE_DEPTH_COMPONENT_NV = $20A6;
  WGL_DEPTH_COMPONENT_NV = $20A7;

  // WGL_NV_render_texture_rectangle
  WGL_BIND_TO_TEXTURE_RECTANGLE_RGB_NV = $20A0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_RGBA_NV = $20A1;
  WGL_TEXTURE_RECTANGLE_NV = $20A2;

  // WGL_NV_present_video
  WGL_NUM_VIDEO_SLOTS_NV = $20F0;

  // WGL_NV_video_output
  WGL_BIND_TO_VIDEO_RGB_NV = $20C0;
  WGL_BIND_TO_VIDEO_RGBA_NV = $20C1;
  WGL_BIND_TO_VIDEO_RGB_AND_DEPTH_NV = $20C2;
  WGL_VIDEO_OUT_COLOR_NV = $20C3;
  WGL_VIDEO_OUT_ALPHA_NV = $20C4;
  WGL_VIDEO_OUT_DEPTH_NV = $20C5;
  WGL_VIDEO_OUT_COLOR_AND_ALPHA_NV = $20C6;
  WGL_VIDEO_OUT_COLOR_AND_DEPTH_NV = $20C7;
  WGL_VIDEO_OUT_FRAME = $20C8;
  WGL_VIDEO_OUT_FIELD_1 = $20C9;
  WGL_VIDEO_OUT_FIELD_2 = $20CA;
  WGL_VIDEO_OUT_STACKED_FIELDS_1_2 = $20CB;
  WGL_VIDEO_OUT_STACKED_FIELDS_2_1 = $20CC;

  // WGL_NV_gpu_affinity
  WGL_ERROR_INCOMPATIBLE_AFFINITY_MASKS_NV = $20D0;
  WGL_ERROR_MISSING_AFFINITY_MASK_NV = $20D1;

  // WGL_NV_video_capture
  WGL_UNIQUE_ID_NV = $20CE;
  WGL_NUM_VIDEO_CAPTURE_SLOTS_NV = $20CF;

  // WGL_NV_multisample_coverage
  WGL_COVERAGE_SAMPLES_NV = $2042;
  WGL_COLOR_SAMPLES_NV = $20B9;

  // WGL_EXT_create_context_es2_profile
  WGL_CONTEXT_ES2_PROFILE_BIT_EXT = $00000004;

  // WGL_NV_DX_interop
  WGL_ACCESS_READ_ONLY_NV = $00000000;
  WGL_ACCESS_READ_WRITE_NV = $00000001;
  WGL_ACCESS_WRITE_DISCARD_NV = $00000002;

  // WIN_draw_range_elements
  GL_MAX_ELEMENTS_VERTICES_WIN = $80E8;
  GL_MAX_ELEMENTS_INDICES_WIN = $80E9;

  // GLX 1.1 and later:
  GLX_VENDOR = 1;
  GLX_VERSION = 2;
  GLX_EXTENSIONS = 3;

  GLX_USE_GL = 1;
  GLX_BUFFER_SIZE = 2;
  GLX_LEVEL = 3;
  GLX_RGBA = 4;
  GLX_DOUBLEBUFFER = 5;
  GLX_STEREO = 6;
  GLX_AUX_BUFFERS = 7;
  GLX_RED_SIZE = 8;
  GLX_GREEN_SIZE = 9;
  GLX_BLUE_SIZE = 10;
  GLX_ALPHA_SIZE = 11;
  GLX_DEPTH_SIZE = 12;
  GLX_STENCIL_SIZE = 13;
  GLX_ACCUM_RED_SIZE = 14;
  GLX_ACCUM_GREEN_SIZE = 15;
  GLX_ACCUM_BLUE_SIZE = 16;
  GLX_ACCUM_ALPHA_SIZE = 17;

  // GLX_VERSION_1_3
  GLX_WINDOW_BIT = $00000001;
  GLX_PIXMAP_BIT = $00000002;
  GLX_PBUFFER_BIT = $00000004;
  GLX_RGBA_BIT = $00000001;
  GLX_COLOR_INDEX_BIT = $00000002;
  GLX_PBUFFER_CLOBBER_MASK = $08000000;
  GLX_FRONT_LEFT_BUFFER_BIT = $00000001;
  GLX_FRONT_RIGHT_BUFFER_BIT = $00000002;
  GLX_BACK_LEFT_BUFFER_BIT = $00000004;
  GLX_BACK_RIGHT_BUFFER_BIT = $00000008;
  GLX_AUX_BUFFERS_BIT = $00000010;
  GLX_DEPTH_BUFFER_BIT = $00000020;
  GLX_STENCIL_BUFFER_BIT = $00000040;
  GLX_ACCUM_BUFFER_BIT = $00000080;
  GLX_CONFIG_CAVEAT = $20;
  GLX_X_VISUAL_TYPE = $22;
  GLX_TRANSPARENT_TYPE = $23;
  GLX_TRANSPARENT_INDEX_VALUE = $24;
  GLX_TRANSPARENT_RED_VALUE = $25;
  GLX_TRANSPARENT_GREEN_VALUE = $26;
  GLX_TRANSPARENT_BLUE_VALUE = $27;
  GLX_TRANSPARENT_ALPHA_VALUE = $28;
  GLX_DONT_CARE = $FFFFFFFF;
  GLX_NONE = $8000;
  GLX_SLOW_CONFIG = $8001;
  GLX_TRUE_COLOR = $8002;
  GLX_DIRECT_COLOR = $8003;
  GLX_PSEUDO_COLOR = $8004;
  GLX_STATIC_COLOR = $8005;
  GLX_GRAY_SCALE = $8006;
  GLX_STATIC_GRAY = $8007;
  GLX_TRANSPARENT_RGB = $8008;
  GLX_TRANSPARENT_INDEX = $8009;
  GLX_VISUAL_ID = $800B;
  GLX_SCREEN = $800C;
  GLX_NON_CONFORMANT_CONFIG = $800D;
  GLX_DRAWABLE_TYPE = $8010;
  GLX_RENDER_TYPE = $8011;
  GLX_X_RENDERABLE = $8012;
  GLX_FBCONFIG_ID = $8013;
  GLX_RGBA_TYPE = $8014;
  GLX_COLOR_INDEX_TYPE = $8015;
  GLX_MAX_PBUFFER_WIDTH = $8016;
  GLX_MAX_PBUFFER_HEIGHT = $8017;
  GLX_MAX_PBUFFER_PIXELS = $8018;
  GLX_PRESERVED_CONTENTS = $801B;
  GLX_LARGEST_PBUFFER = $801C;
  GLX_WIDTH = $801D;
  GLX_HEIGHT = $801E;
  GLX_EVENT_MASK = $801F;
  GLX_DAMAGED = $8020;
  GLX_SAVED = $8021;
  GLX_WINDOW = $8022;
  GLX_PBUFFER = $8023;
  GLX_PBUFFER_HEIGHT = $8040;
  GLX_PBUFFER_WIDTH = $8041;

  // GLX_VERSION_1_4
  GLX_SAMPLE_BUFFERS = 100000;
  GLX_SAMPLES = 100001;

  // GLX_ARB_multisample
  GLX_SAMPLE_BUFFERS_ARB = 100000;
  GLX_SAMPLES_ARB = 100001;

  // GLX_ARB_robustness_application_isolation
  GLX_CONTEXT_RESET_ISOLATION_BIT_ARB = $00000008;

  // GLX_ARB_fbconfig_float
  GLX_RGBA_FLOAT_TYPE_ARB = $20B9;
  GLX_RGBA_FLOAT_BIT_ARB = $00000004;

  // GLX_ARB_context_flush_control
  GLX_CONTEXT_RELEASE_BEHAVIOR_ARB = $2097;
  GLX_CONTEXT_RELEASE_BEHAVIOR_NONE_ARB = 0;
  GLX_CONTEXT_RELEASE_BEHAVIOR_FLUSH_ARB = $2098;

  // GLX_ARB_create_context
  GLX_CONTEXT_DEBUG_BIT_ARB = $00000001;
  GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = $00000002;
  GLX_CONTEXT_MAJOR_VERSION_ARB = $2091;
  GLX_CONTEXT_MINOR_VERSION_ARB = $2092;
  GLX_CONTEXT_FLAGS_ARB = $2094;

  // GLX_ARB_create_context_profile
  GLX_CONTEXT_CORE_PROFILE_BIT_ARB = $00000001;
  GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = $00000002;
  GLX_CONTEXT_PROFILE_MASK_ARB = $9126;

  // GLX_ARB_vertex_buffer_object
  GLX_CONTEXT_ALLOW_BUFFER_BYTE_ORDER_MISMATCH_ARB = $2095;

  // GLX_ARB_framebuffer_sRGB
  GLX_FRAMEBUFFER_SRGB_CAPABLE_ARB = $20B2;

  // GLX_ARB_create_context_robustness
  GLX_CONTEXT_ROBUST_ACCESS_BIT_ARB = $00000004;
  GLX_LOSE_CONTEXT_ON_RESET_ARB = $8252;
  GLX_CONTEXT_RESET_NOTIFICATION_STRATEGY_ARB = $8256;
  GLX_NO_RESET_NOTIFICATION_ARB = $8261;

  // GLX_EXT_visual_info
  GLX_X_VISUAL_TYPE_EXT = $22;
  GLX_TRANSPARENT_TYPE_EXT = $23;
  GLX_TRANSPARENT_INDEX_VALUE_EXT = $24;
  GLX_TRANSPARENT_RED_VALUE_EXT = $25;
  GLX_TRANSPARENT_GREEN_VALUE_EXT = $26;
  GLX_TRANSPARENT_BLUE_VALUE_EXT = $27;
  GLX_TRANSPARENT_ALPHA_VALUE_EXT = $28;
  GLX_NONE_EXT = $8000;
  GLX_TRUE_COLOR_EXT = $8002;
  GLX_DIRECT_COLOR_EXT = $8003;
  GLX_PSEUDO_COLOR_EXT = $8004;
  GLX_STATIC_COLOR_EXT = $8005;
  GLX_GRAY_SCALE_EXT = $8006;
  GLX_STATIC_GRAY_EXT = $8007;
  GLX_TRANSPARENT_RGB_EXT = $8008;
  GLX_TRANSPARENT_INDEX_EXT = $8009;

  // GLX_EXT_visual_rating
  GLX_VISUAL_CAVEAT_EXT = $20;
  GLX_SLOW_VISUAL_EXT = $8001;
  GLX_NON_CONFORMANT_VISUAL_EXT = $800D;
  (* reuse GLX_NONE_EXT *)

  // GLX_EXT_import_context
  GLX_SHARE_CONTEXT_EXT = $800A;
  GLX_VISUAL_ID_EXT = $800B;
  GLX_SCREEN_EXT = $800C;

  // GLX_EXT_fbconfig_packed_float

  GLX_RGBA_UNSIGNED_FLOAT_TYPE_EXT = $20B1;
  GLX_RGBA_UNSIGNED_FLOAT_BIT_EXT = $00000008;

  // GLX_EXT_framebuffer_sRGB
  (* GLX_FRAMEBUFFER_SRGB_CAPABLE_EXT = $20B2; *)

  // GLX_EXT_texture_from_pixmap
  GLX_TEXTURE_1D_BIT_EXT = $00000001;
  GLX_TEXTURE_2D_BIT_EXT = $00000002;
  GLX_TEXTURE_RECTANGLE_BIT_EXT = $00000004;
  GLX_BIND_TO_TEXTURE_RGB_EXT = $20D0;
  GLX_BIND_TO_TEXTURE_RGBA_EXT = $20D1;
  GLX_BIND_TO_MIPMAP_TEXTURE_EXT = $20D2;
  GLX_BIND_TO_TEXTURE_TARGETS_EXT = $20D3;
  GLX_Y_INVERTED_EXT = $20D4;
  GLX_TEXTURE_FORMAT_EXT = $20D5;
  GLX_TEXTURE_TARGET_EXT = $20D6;
  GLX_MIPMAP_TEXTURE_EXT = $20D7;
  GLX_TEXTURE_FORMAT_NONE_EXT = $20D8;
  GLX_TEXTURE_FORMAT_RGB_EXT = $20D9;
  GLX_TEXTURE_FORMAT_RGBA_EXT = $20DA;
  GLX_TEXTURE_1D_EXT = $20DB;
  GLX_TEXTURE_2D_EXT = $20DC;
  GLX_TEXTURE_RECTANGLE_EXT = $20DD;
  GLX_FRONT_LEFT_EXT = $20DE;
  GLX_FRONT_RIGHT_EXT = $20DF;
  GLX_BACK_LEFT_EXT = $20E0;
  GLX_BACK_RIGHT_EXT = $20E1;
  GLX_FRONT_EXT = GLX_FRONT_LEFT_EXT;
  GLX_BACK_EXT = GLX_BACK_LEFT_EXT;
  GLX_AUX0_EXT = $20E2;
  GLX_AUX1_EXT = $20E3;
  GLX_AUX2_EXT = $20E4;
  GLX_AUX3_EXT = $20E5;
  GLX_AUX4_EXT = $20E6;
  GLX_AUX5_EXT = $20E7;
  GLX_AUX6_EXT = $20E8;
  GLX_AUX7_EXT = $20E9;
  GLX_AUX8_EXT = $20EA;
  GLX_AUX9_EXT = $20EB;

  // GLX_EXT_swap_control
  GLX_SWAP_INTERVAL_EXT = $20F1;
  GLX_MAX_SWAP_INTERVAL_EXT = $20F2;

  // GLX_EXT_create_context_es2_profile
  GLX_CONTEXT_ES2_PROFILE_BIT_EXT = $00000004;

  // GL_EXT_Late_Swaps
  GLX_LATE_SWAPS_TEAR_EXT = $20F3;

  // GLU
  GLU_INVALID_ENUM = 100900;
  GLU_INVALID_VALUE = 100901;
  GLU_OUT_OF_MEMORY = 100902;
  GLU_INCOMPATIBLE_GL_VERSION = 100903;
  GLU_VERSION = 100800;
  GLU_EXTENSIONS = 100801;
  GLU_TRUE = GL_TRUE;
  GLU_FALSE = GL_FALSE;
  GLU_SMOOTH = 100000;
  GLU_FLAT = 100001;
  GLU_NONE = 100002;
  GLU_POINT = 100010;
  GLU_LINE = 100011;
  GLU_FILL = 100012;
  GLU_SILHOUETTE = 100013;
  GLU_OUTSIDE = 100020;
  GLU_INSIDE = 100021;
  GLU_TESS_MAX_COORD = 1.0E150;
  GLU_TESS_WINDING_RULE = 100140;
  GLU_TESS_BOUNDARY_ONLY = 100141;
  GLU_TESS_TOLERANCE = 100142;
  GLU_TESS_WINDING_ODD = 100130;
  GLU_TESS_WINDING_NONZERO = 100131;
  GLU_TESS_WINDING_POSITIVE = 100132;
  GLU_TESS_WINDING_NEGATIVE = 100133;
  GLU_TESS_WINDING_ABS_GEQ_TWO = 100134;
  GLU_TESS_BEGIN = 100100;
  GLU_TESS_VERTEX = 100101;
  GLU_TESS_END = 100102;
  GLU_TESS_ERROR = 100103;
  GLU_TESS_EDGE_FLAG = 100104;
  GLU_TESS_COMBINE = 100105;
  GLU_TESS_BEGIN_DATA = 100106;
  GLU_TESS_VERTEX_DATA = 100107;
  GLU_TESS_END_DATA = 100108;
  GLU_TESS_ERROR_DATA = 100109;
  GLU_TESS_EDGE_FLAG_DATA = 100110;
  GLU_TESS_COMBINE_DATA = 100111;
  GLU_TESS_ERROR1 = 100151;
  GLU_TESS_ERROR2 = 100152;
  GLU_TESS_ERROR3 = 100153;
  GLU_TESS_ERROR4 = 100154;
  GLU_TESS_ERROR5 = 100155;
  GLU_TESS_ERROR6 = 100156;
  GLU_TESS_ERROR7 = 100157;
  GLU_TESS_ERROR8 = 100158;
  GLU_TESS_MISSING_BEGIN_POLYGON = GLU_TESS_ERROR1;
  GLU_TESS_MISSING_BEGIN_CONTOUR = GLU_TESS_ERROR2;
  GLU_TESS_MISSING_END_POLYGON = GLU_TESS_ERROR3;
  GLU_TESS_MISSING_END_CONTOUR = GLU_TESS_ERROR4;
  GLU_TESS_COORD_TOO_LARGE = GLU_TESS_ERROR5;
  GLU_TESS_NEED_COMBINE_CALLBACK = GLU_TESS_ERROR6;
  GLU_AUTO_LOAD_MATRIX = 100200;
  GLU_CULLING = 100201;
  GLU_SAMPLING_TOLERANCE = 100203;
  GLU_DISPLAY_MODE = 100204;
  GLU_PARAMETRIC_TOLERANCE = 100202;
  GLU_SAMPLING_METHOD = 100205;
  GLU_U_STEP = 100206;
  GLU_V_STEP = 100207;
  GLU_PATH_LENGTH = 100215;
  GLU_PARAMETRIC_ERROR = 100216;
  GLU_DOMAIN_DISTANCE = 100217;
  GLU_MAP1_TRIM_2 = 100210;
  GLU_MAP1_TRIM_3 = 100211;
  GLU_OUTLINE_POLYGON = 100240;
  GLU_OUTLINE_PATCH = 100241;
  GLU_NURBS_ERROR1 = 100251;
  GLU_NURBS_ERROR2 = 100252;
  GLU_NURBS_ERROR3 = 100253;
  GLU_NURBS_ERROR4 = 100254;
  GLU_NURBS_ERROR5 = 100255;
  GLU_NURBS_ERROR6 = 100256;
  GLU_NURBS_ERROR7 = 100257;
  GLU_NURBS_ERROR8 = 100258;
  GLU_NURBS_ERROR9 = 100259;
  GLU_NURBS_ERROR10 = 100260;
  GLU_NURBS_ERROR11 = 100261;
  GLU_NURBS_ERROR12 = 100262;
  GLU_NURBS_ERROR13 = 100263;
  GLU_NURBS_ERROR14 = 100264;
  GLU_NURBS_ERROR15 = 100265;
  GLU_NURBS_ERROR16 = 100266;
  GLU_NURBS_ERROR17 = 100267;
  GLU_NURBS_ERROR18 = 100268;
  GLU_NURBS_ERROR19 = 100269;
  GLU_NURBS_ERROR20 = 100270;
  GLU_NURBS_ERROR21 = 100271;
  GLU_NURBS_ERROR22 = 100272;
  GLU_NURBS_ERROR23 = 100273;
  GLU_NURBS_ERROR24 = 100274;
  GLU_NURBS_ERROR25 = 100275;
  GLU_NURBS_ERROR26 = 100276;
  GLU_NURBS_ERROR27 = 100277;
  GLU_NURBS_ERROR28 = 100278;
  GLU_NURBS_ERROR29 = 100279;
  GLU_NURBS_ERROR30 = 100280;
  GLU_NURBS_ERROR31 = 100281;
  GLU_NURBS_ERROR32 = 100282;
  GLU_NURBS_ERROR33 = 100283;
  GLU_NURBS_ERROR34 = 100284;
  GLU_NURBS_ERROR35 = 100285;
  GLU_NURBS_ERROR36 = 100286;
  GLU_NURBS_ERROR37 = 100287;
  GLU_CW = 100120;
  GLU_CCW = 100121;
  GLU_INTERIOR = 100122;
  GLU_EXTERIOR = 100123;
  GLU_UNKNOWN = 100124;
  GLU_BEGIN = GLU_TESS_BEGIN;
  GLU_VERTEX = GLU_TESS_VERTEX;
  GLU_END = GLU_TESS_END;
  GLU_ERROR = GLU_TESS_ERROR;
  GLU_EDGE_FLAG = GLU_TESS_EDGE_FLAG;

type
  {$IFDEF SUPPORT_WGL}
  // WGL
  // WGL - ARB
  // WGL_buffer_region (ARB #4)
  PFNWGLCREATEBUFFERREGIONARBPROC = function(DC: HDC; iLayerPlane: Integer; uType: GLenum) : Integer; stdcall;
  PFNWGLDELETEBUFFERREGIONARBPROC = procedure(hRegion: Integer); stdcall;
  PFNWGLSAVEBUFFERREGIONARBPROC = function(hRegion: Integer; x, y, width, height: Integer): BOOL; stdcall;
  PFNWGLRESTOREBUFFERREGIONARBPROC = function(hRegion: Integer; x, y, width, height: Integer;
   xSrc, ySrc: Integer): BOOL; stdcall;

  // WGL_ARB_extensions_string (ARB #8)
  PFNWGLGETEXTENSIONSSTRINGARBPROC = function(DC: HDC): PGLChar; stdcall;

  // WGL_ARB_pixel_format (ARB #9)
  PFNWGLGETPIXELFORMATATTRIBIVARBPROC = function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: GLenum;
   const piAttributes: PGLint; piValues : PGLint) : BOOL; stdcall;
  PFNWGLGETPIXELFORMATATTRIBFVARBPROC = function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: GLenum;
   const piAttributes: PGLint; piValues: PGLFloat) : BOOL; stdcall;
  PFNWGLCHOOSEPIXELFORMATARBPROC = function(DC: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLFloat;
   nMaxFormats: GLuint; piFormats: PGLint; nNumFormats: PGLenum) : BOOL; stdcall;

  // WGL_make_current_read (ARB #10)
  PFNWGLMAKECONTEXTCURRENTARBPROC = function(hDrawDC: HDC; hReadDC: HDC; _hglrc: HGLRC): BOOL; stdcall;
  PFNWGLGETCURRENTREADDCARBPROC = function(): HDC; stdcall;

  // WGL_ARB_pbuffer (ARB #11)
  PFNWGLCREATEPBUFFERARBPROC = function(DC: HDC; iPixelFormat: GLInt; iWidth, iHeight : GLInt;
   const piAttribList: PGLint) : HPBUFFERARB; stdcall;
  PFNWGLGETPBUFFERDCARBPROC = function(hPbuffer: HPBUFFERARB) : HDC; stdcall;
  PFNWGLRELEASEPBUFFERDCARBPROC = function(hPbuffer: HPBUFFERARB; DC: HDC) : Integer; stdcall;
  PFNWGLDESTROYPBUFFERARBPROC = function(hPbuffer: HPBUFFERARB): BOOL; stdcall;
  PFNWGLQUERYPBUFFERARBPROC = function(hPbuffer: HPBUFFERARB; iAttribute : Integer;
   piValue: PGLint) : BOOL; stdcall;

  // WGL_ARB_render_texture (ARB #20)
  PFNWGLBINDTEXIMAGEARBPROC = function(hPbuffer: HPBUFFERARB; iBuffer: Integer): BOOL; stdcall;
  PFNWGLRELEASETEXIMAGEARBPROC = function(hpBuffer: HPBUFFERARB; iBuffer: Integer): BOOL; stdcall;
  PFNWGLSETPBUFFERATTRIBARBPROC = function(hpBuffer: HPBUFFERARB; const piAttribList: PGLint): BOOL; stdcall;

  // WGL_ARB_create_context (ARB #55)
  PFNWGLCREATECONTEXTATTRIBSARBPROC = function(DC: HDC; hShareContext: HGLRC;
           attribList: PGLint):HGLRC; stdcall;

  // WGL - EXT
  // WGL_EXT_swap_control (EXT #172)
  PFNWGLSWAPINTERVALEXTPROC = function(interval : Integer) : BOOL; stdcall;
  PFNWGLGETSWAPINTERVALEXTPROC = function : Integer; stdcall;

    // GL_NV_vertex_array_range (EXT #190)
  PFNWGLALLOCATEMEMORYNVPROC =  function(size: GLsizei; readFrequency, writeFrequency, priority: Single): Pointer; stdcall;
  PFNWGLFREEMEMORYNVPROC = function(size: GLsizei; readFrequency, writeFrequency, priority: Single): Pointer; stdcall;

  // WGL_NV_gpu_affinity (EXT #355)
  PFNWGLENUMGPUSNVPROC = function(iGpuIndex: Cardinal; var hGpu: HGPUNV): Boolean; stdcall;
  PFNWGLENUMGPUDEVICESNVPROC = function(hGpu: HGPUNV; iDeviceIndex: Cardinal; lpGpuDevice: PGPUDevice): Boolean; stdcall;
  PFNWGLCREATEAFFINITYDCNVPROC = function(hGpuList: PHGPUNV): HDC; stdcall;
  PFNWGLENUMGPUSFROMAFFINITYDCNVPROC = function(hAffinityDC: HDC; iGpuIndex: Cardinal; var hGpu: HGPUNV): Boolean; stdcall;
  PFNWGLDELETEDCNVPROC = function(hdc: HDC): Boolean; stdcall;

  // WGL_NV_DX_interop (EXT #407)
  PFNWGLDXSETRESOURCESHAREHANDLEPROC = function (dxObject: Pointer; shareHandle: THandle): BOOL; stdcall;
  PFNWGLDXOPENDEVICEPROC = function(dxDevice: Pointer): THandle; stdcall;
  PFNWGLDXCLOSEDEVICEPROC = function(hDevice: THandle): BOOL; stdcall;
  PFNWGLDXREGISTEROBJECTPROC = function(hDevice: THandle; dxObject: Pointer;
                                name: GLuint; atype: GLuint; access: GLuint): THandle; stdcall;
  PFNWGLDXUNREGISTEROBJECTPROC = function(hDevice: THandle; hObject: THandle): BOOL; stdcall;
  PFNWGLDXOBJECTACCESSPROC = function(hObject: THandle; access: GLenum): BOOL; stdcall;
  PFNWGLDXLOCKOBJECTSPROC = function(hDevice: THandle; count: GLint; hObjects: PHandle): BOOL; stdcall;
  PFNWGLDXUNLOCKOBJECTSNVPROC = function (hDevice: THandle; count: GLint; hObjects: PHandle): BOOL; stdcall;
  {$ENDIF}



type
  EOpenGLError = class(Exception);

  TVXExtensionsAndEntryPoints = class
  private
    FBuffer: string;
    FInitialized: Boolean;
    FDebug: Boolean;
    FDebugIds: PGLuint;
    function CheckExtension(const Extension: string): Boolean;
{$IFDEF SUPPORT_WGL}
    procedure ReadWGLExtensions;
    procedure ReadWGLImplementationProperties;
{$ENDIF}
{$IFDEF SUPPORT_GLX}
    procedure ReadGLXExtensions;
    procedure ReadGLXImplementationProperties;
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
    // ###########################################################
    // function and procedure definitions for
    // ARB approved WGL extensions
    // ###########################################################

    // ARB approved WGL extension checks
    WGL_ARB_buffer_region, W_ARB_create_context, W_ARB_create_context_profile,
      W_ARB_extensions_string, W_ARB_framebuffer_sRGB, W_ARB_make_current_read,
      W_ARB_multisample, W_ARB_pbuffer, W_ARB_pixel_format,
      W_ARB_pixel_format_float, W_ARB_render_texture,

    // Vendor/EXT WGL extension checks
    WGL_ATI_pixel_format_float, W_EXT_framebuffer_sRGB,
      W_EXT_pixel_format_packed_float, W_EXT_swap_control, W_NV_gpu_affinity,
      W_EXT_create_context_es2_profile, W_NV_DX_interop,
      W_NV_DX_interop2: Boolean;

    // WGL_buffer_region (ARB #4)
    wglCreateBufferRegionARB: PFNWGLCREATEBUFFERREGIONARBPROC;
    wglDeleteBufferRegionARB: PFNWGLDELETEBUFFERREGIONARBPROC;
    wglSaveBufferRegionARB: PFNWGLSAVEBUFFERREGIONARBPROC;
    wglRestoreBufferRegionARB: PFNWGLRESTOREBUFFERREGIONARBPROC;

    // WGL_ARB_extensions_string (ARB #8)
    wglGetExtensionsStringARB: PFNWGLGETEXTENSIONSSTRINGARBPROC;

    // WGL_ARB_pixel_format (ARB #9)
    wglGetPixelFormatAttribivARB: PFNWGLGETPIXELFORMATATTRIBIVARBPROC;
    wglGetPixelFormatAttribfvARB: PFNWGLGETPIXELFORMATATTRIBFVARBPROC;
    wglChoosePixelFormatARB: PFNWGLCHOOSEPIXELFORMATARBPROC;

    // WGL_make_current_read (ARB #10)
    wglMakeContextCurrentARB: PFNWGLMAKECONTEXTCURRENTARBPROC;
    wglGetCurrentReadDCARB: PFNWGLGETCURRENTREADDCARBPROC;

    // WGL_ARB_pbuffer (ARB #11)
    wglCreatePbufferARB: PFNWGLCREATEPBUFFERARBPROC;
    wglGetPbufferDCARB: PFNWGLGETPBUFFERDCARBPROC;
    wglReleasePbufferDCARB: PFNWGLRELEASEPBUFFERDCARBPROC;
    wglDestroyPbufferARB: PFNWGLDESTROYPBUFFERARBPROC;
    wglQueryPbufferARB: PFNWGLQUERYPBUFFERARBPROC;

    // WGL_ARB_render_texture (ARB #20)
    wglBindTexImageARB: PFNWGLBINDTEXIMAGEARBPROC;
    wglReleaseTexImageARB: PFNWGLRELEASETEXIMAGEARBPROC;
    wglSetPbufferAttribARB: PFNWGLSETPBUFFERATTRIBARBPROC;

    // WGL_ARB_create_context (ARB #55)
    wglCreateContextAttribsARB: PFNWGLCREATECONTEXTATTRIBSARBPROC;

    // WGL_EXT_swap_control (EXT #172)
    wglSwapIntervalEXT : PFNWGLSwapIntervalEXTPROC;
    wglGetSwapIntervalEXT : PFNWGLGetSwapIntervalEXTPROC;

    // GL_NV_vertex_array_range (EXT #190)
    wglAllocateMemoryNV: PFNWGLALLOCATEMEMORYNVPROC;
    wglFreeMemoryNV: PFNWGLFREEMEMORYNVPROC;

    // WGL_NV_gpu_affinity
    wglEnumGpusNV: PFNWGLENUMGPUSNVPROC;
    wglEnumGpuDevicesNV: PFNWGLENUMGPUDEVICESNVPROC;
    wglCreateAffinityDCNV: PFNWGLCREATEAFFINITYDCNVPROC;
    wglEnumGpusFromAffinityDCNV: PFNWGLENUMGPUSFROMAFFINITYDCNVPROC;
    wglDeleteDCNV: PFNWGLDELETEDCNVPROC;

    // WGL_NV_DX_interop (EXT #407)
    wglDXSetResourceShareHandleNV: PFNWGLDXSETRESOURCESHAREHANDLEPROC;
    wglDXOpenDeviceNV: PFNWGLDXOPENDEVICEPROC;
    wglDXCloseDeviceNV: PFNWGLDXCLOSEDEVICEPROC;
    wglDXRegisterObjectNV: PFNWGLDXREGISTEROBJECTPROC;
    wglDXUnregisterObjectNV: PFNWGLDXUNREGISTEROBJECTPROC;
    wglDXObjectAccessNV: PFNWGLDXOBJECTACCESSPROC;
    wglDXLockObjectsNV: PFNWGLDXLOCKOBJECTSPROC;
    wglDXUnlockObjectsNV: PFNWGLDXUNLOCKOBJECTSNVPROC;

   {$ENDIF}



    {$IFDEF SUPPORT_GLX}
    //---------------------------------------------------------------------
    // function and procedure definitions for
    // ARB approved GLX extensions
    // ---------------------------------------------------------------------
    // GLX extension checks
    X_VERSION_1_1, X_VERSION_1_2, X_VERSION_1_3, X_VERSION_1_4,
      X_ARB_create_context, X_ARB_create_context_profile,
      X_ARB_framebuffer_sRGB, X_ARB_multisample, X_EXT_framebuffer_sRGB,
      X_EXT_fbconfig_packed_float, X_SGIS_multisample, X_EXT_visual_info,
      X_SGI_swap_control, X_SGI_video_sync, X_SGI_make_current_read,
      X_SGIX_video_source, X_EXT_visual_rating, X_EXT_import_context,
      X_SGIX_fbconfig, X_SGIX_pbuffer, X_SGI_cushion, X_SGIX_video_resize,
      X_SGIX_dmbuffer, X_SGIX_swap_group, X_SGIX_swap_barrier,
      X_SGIS_blended_overlay, X_SGIS_shared_multisample,
      X_SUN_get_transparent_index, X_3DFX_multisample, X_MESA_copy_sub_buffer,
      X_MESA_pixmap_colormap, X_MESA_release_buffers, X_MESA_set_3dfx_mode,
      X_SGIX_visual_select_group, X_SGIX_hyperpipe,
      X_NV_multisample_coverage: Boolean;

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
    glXCreateContextAttribsARB: PFNGLXCREATECONTEXTATTRIBSARBPROC;
    {$ENDIF}

    {$IFDEF SUPPORT_GLX}
    // ###########################################################
    // function and procedure definitions for
    // Vendor/EXT GLX extensions
    // ###########################################################

    // X_SGI_swap_control (EXT #40)
    glxSwapIntervalSGI: PFNGLXSWAPINTERVALSGIPROC;
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
    glxMakeCurrentReadSGI: PFNGLXMAKECURRENTREADSGIPROC;
    glxGetCurrentReadDrawableSGI: PFNGLXGETCURRENTREADDRAWABLESGIPROC;
    glxGetFBConfigAttribSGIX: PFNGLXGETFBCONFIGATTRIBSGIXPROC;
    glxChooseFBConfigSGIX: PFNGLXCHOOSEFBCONFIGSGIXPROC;
    glxCreateGLXPixmapWithConfigSGIX: PFNGLXCREATEGLXPIXMAPWITHCONFIGSGIXPROC;
    glxCreateContextWithConfigSGIX: PFNGLXCREATECONTEXTWITHCONFIGSGIXPROC;
    glxGetVisualFromFBConfigSGIX: PFNGLXGETVISUALFROMFBCONFIGSGIXPROC;
    glxGetFBConfigFromVisualSGIX: PFNGLXGETFBCONFIGFROMVISUALSGIXPROC;
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
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EGetDisplay : function(display_id:EGLNativeDisplayType):EGLDisplay;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EInitialize : function(dpy:EGLDisplay; major:pEGLint; minor:pEGLint):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    ETerminate : function(dpy:EGLDisplay):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EQueryString : function(dpy:EGLDisplay; name:EGLint):pchar;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EGetConfigs : function(dpy:EGLDisplay; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EChooseConfig : function(dpy:EGLDisplay; attrib_list:pEGLint; configs:pEGLConfig; config_size:EGLint; num_config:pEGLint):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EGetConfigAttrib : function(dpy:EGLDisplay; config:EGLConfig; attribute:EGLint; value:pEGLint):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    ECreateWindowSurface : function(dpy:EGLDisplay; config:EGLConfig; win:EGLNativeWindowType; attrib_list:pEGLint):EGLSurface;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    ECreatePbufferSurface : function(dpy:EGLDisplay; config:EGLConfig; attrib_list:pEGLint):EGLSurface;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    ECreatePixmapSurface : function(dpy:EGLDisplay; config:EGLConfig; pixmap:EGLNativePixmapType; attrib_list:pEGLint):EGLSurface;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EDestroySurface : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EQuerySurface : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:pEGLint):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EBindAPI : function(api:EGLenum):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EQueryAPI : function:EGLenum;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EWaitClient : function:EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EReleaseThread : function:EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    ECreatePbufferFromClientBuffer : function(dpy:EGLDisplay; buftype:EGLenum; buffer:EGLClientBuffer; config:EGLConfig; attrib_list:pEGLint):EGLSurface;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    ESurfaceAttrib : function(dpy:EGLDisplay; surface:EGLSurface; attribute:EGLint; value:EGLint):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EBindTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EReleaseTexImage : function(dpy:EGLDisplay; surface:EGLSurface; buffer:EGLint):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    ESwapInterval : function(dpy:EGLDisplay; interval:EGLint):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    ECreateContext : function(dpy:EGLDisplay; config:EGLConfig; share_context:EGLContext; attrib_list:pEGLint):EGLContext;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EDestroyContext : function(dpy:EGLDisplay; ctx:EGLContext):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EMakeCurrent : function(dpy:EGLDisplay; draw:EGLSurface; read:EGLSurface; ctx:EGLContext):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EGetCurrentContext : function:EGLContext;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EGetCurrentSurface : function(readdraw:EGLint):EGLSurface;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EGetCurrentDisplay : function:EGLDisplay;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EQueryContext : function(dpy:EGLDisplay; ctx:EGLContext; attribute:EGLint; value:pEGLint):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EWaitGL : function:EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    EWaitNative : function(engine:EGLint):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    ESwapBuffers : function(dpy:EGLDisplay; surface:EGLSurface):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    ECopyBuffers : function(dpy:EGLDisplay; surface:EGLSurface; target:EGLNativePixmapType):EGLBoolean;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    {$ENDIF EGL_SUPPORT}

    {$IFDEF VXS_REGIONS}{$ENDREGION}{$ENDIF}

    {$IFDEF VXS_REGIONS}{$REGION 'locate functions/procedures for OpenGL Utility (GLU) extensions'} {$ENDIF}

    // ###########################################################
    // locate functions and procedures for
    // GLU extensions
    // ###########################################################
    (*
    gluNurbsCallbackDataEXT: procedure(nurb: PGLUnurbs; userData: Pointer);
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    gluNewNurbsTessellatorEXT: function: PGLUnurbs;
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    gluDeleteNurbsTessellatorEXT: procedure(nurb: PGLUnurbs);
    {$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
    *)
    constructor Create;
    procedure Initialize(ATemporary: Boolean = False);
    procedure Close;
    property IsInitialized: Boolean read FInitialized;
    property DebugMode: Boolean read FDebug write FDebug;
  end;

//Windows OpenGL (WGL) support functions
{$IFDEF SUPPORT_WGL}

function wglGetProcAddress(ProcName: PGLChar): Pointer; stdcall;
  external opengl32;
function wglCopyContext(p1: HGLRC; p2: HGLRC; p3: Cardinal): BOOL; stdcall;
  external opengl32;
function wglCreateContext(DC: HDC): HGLRC; stdcall; external opengl32;
function wglCreateLayerContext(p1: HDC; p2: Integer): HGLRC; stdcall;
  external opengl32;
function wglDeleteContext(p1: HGLRC): BOOL; stdcall; external opengl32;
function wglDescribeLayerPlane(p1: HDC; p2, p3: Integer; p4: Cardinal;
  var p5: TLayerPlaneDescriptor): BOOL; stdcall; external opengl32;
function wglGetCurrentContext: HGLRC; stdcall; external opengl32;
function wglGetCurrentDC: HDC; stdcall; external opengl32;
function wglGetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr)
  : Integer; stdcall; external opengl32;
function wglMakeCurrent(DC: HDC; p2: HGLRC): BOOL; stdcall; external opengl32;
function wglRealizeLayerPalette(p1: HDC; p2: Integer; p3: BOOL): BOOL; stdcall;
  external opengl32;
function wglSetLayerPaletteEntries(p1: HDC; p2, p3, p4: Integer; var pcr)
  : Integer; stdcall; external opengl32;
function wglShareLists(p1, p2: HGLRC): BOOL; stdcall; external opengl32;
function wglSwapLayerBuffers(p1: HDC; p2: Cardinal): BOOL; stdcall;
  external opengl32;
function wglSwapMultipleBuffers(p1: UINT; const p2: PWGLSwap): DWORD; stdcall;
  external opengl32;
function wglUseFontBitmapsA(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  external opengl32;
function wglUseFontOutlinesA(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single;
  p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
function wglUseFontBitmapsW(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  external opengl32;
function wglUseFontOutlinesW(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single;
  p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall; external opengl32;
function wglUseFontBitmaps(DC: HDC; p2, p3, p4: DWORD): BOOL; stdcall;
  external opengl32 Name 'wglUseFontBitmapsA';
function wglUseFontOutlines(p1: HDC; p2, p3, p4: DWORD; p5, p6: Single;
  p7: Integer; p8: PGlyphMetricsFloat): BOOL; stdcall;
  external opengl32 Name 'wglUseFontOutlinesA';
{$ENDIF}

//OpenGL Extension to the X Window System (GLX) support functions
  {$IFDEF SUPPORT_GLX}
  // GLX 1.0
  function glXGetProcAddress(const Name: PAnsiChar): Pointer; cdecl; external opengl32;
  function glXGetProcAddressARB(const Name: PAnsiChar): Pointer; cdecl; external opengl32;
  function glXChooseVisual(dpy: PDisplay; screen: GLint; attribList: PGLint): PXVisualInfo; cdecl; external opengl32;
  function glXCreateContext(dpy: PDisplay; vis: PXVisualInfo; shareList: GLXContext; direct: GLboolean): GLXContext; cdecl; external opengl32;
  procedure glXDestroyContext(dpy: PDisplay; ctx: GLXContext); cdecl; external opengl32;
  function glXMakeCurrent(dpy: PDisplay; drawable: GLXDrawable; ctx: GLXContext): GLboolean; cdecl; external opengl32;
  procedure glXCopyContext(dpy: PDisplay; src: GLXContext; dst: GLXContext; mask: GLuint); cdecl; external opengl32;
  procedure glXSwapBuffers(dpy: PDisplay; drawable: GLXDrawable); cdecl; external opengl32;
  function glXCreateGLXPixmap(dpy: PDisplay; visual: PXVisualInfo;
    pixmap: GLXPixmap): GLXPixmap; cdecl; external opengl32;
  procedure glXDestroyGLXPixmap(dpy: PDisplay; pixmap: GLXPixmap); cdecl; external opengl32;
  function glXQueryExtension(dpy: PDisplay; errorb: PGLint; event: PGLint): GLboolean; cdecl; external opengl32;
  function glXQueryVersion(dpy: PDisplay; maj: PGLint; min: PGLint): GLboolean; cdecl; external opengl32;
  function glXIsDirect(dpy: PDisplay; ctx: GLXContext): GLboolean; cdecl; external opengl32;
  function glXGetConfig(dpy: PDisplay; visual: PXVisualInfo; attrib: GLint; Value: PGLint): GLint; cdecl; external opengl32;
  function glXGetCurrentContext: GLXContext; cdecl; external opengl32;
  function glXGetCurrentDrawable: GLXDrawable; cdecl; external opengl32;
  procedure glXWaitGL; cdecl; external opengl32;
  procedure glXWaitX; cdecl; external opengl32;
  procedure glXUseXFont(font: XFont; First: GLint; Count: GLint; list: GLint); cdecl; external opengl32;
  function glXQueryExtensionsString(dpy: PDisplay; screen: GLint): PGLChar; cdecl; external opengl32;
  function glXQueryServerString(dpy: PDisplay; screen: GLint; Name: GLint): PGLChar; cdecl; external opengl32;
  function glXGetClientString(dpy: PDisplay; Name: GLint): PGLChar; cdecl; external opengl32;
  function glXGetCurrentDisplay: PDisplay; cdecl; external opengl32;
  {$ENDIF}



function GetProcAddressGLLib(ProcName: PGLChar): Pointer;
function GetProcAddressGLS(ProcName: PGLChar): Pointer;

procedure CloseOpenGL;
function InitOpenGL: Boolean;
function InitOpenGLFromLibrary(const GLName, GLUName: string): Boolean;
function IsOpenGLInitialized: Boolean;

// compatibility routines
procedure UnloadOpenGL;
function LoadOpenGL: Boolean;
function LoadOpenGLFromLibrary(GLName, GLUName: string): Boolean;
function IsOpenGLLoaded: Boolean;

function IsMesaGL: Boolean;
procedure TrimAndSplitVersionString(buffer: string; var max, min: Integer);
function IsVersionMet(MajorVersion, MinorVersion, actualMajorVersion,
  actualMinorVersion: Integer): Boolean;

procedure ClearOpenGLError;
procedure CheckOpenGLError;

//------------------------------------------------------------------
implementation
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

function GetProcAddressGLS(ProcName: PGLChar): Pointer;
begin
{$IFNDEF EGL_SUPPORT}
  Result := wglGetProcAddress(ProcName);
{$ELSE}
  Result := GetProcAddress(EGL2Handle, ProcName);
{$ENDIF}
end;

{$IFDEF EGL_SUPPORT}

function GetProcAddressEGL(ProcName: PGLChar): Pointer;
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
{$IFDEF EGL_SUPPORT}
  EGL2Handle: TLibHandle = 0;
{$ENDIF}

function GetProcAddressGLS(ProcName: PGLChar): Pointer;
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

{$IFDEF EGL_SUPPORT}

function GetProcAddressEGL(ProcName: PGLChar): Pointer;
begin
  Result := GetProcAddress(EGLHandle, ProcName);
end;
{$ENDIF}


{$ENDIF UNIX}

function GetProcAddressGLLib(ProcName: PGLChar): Pointer;
begin
  Result := GetProcAddress(GLHandle, ProcName);
end;

var
  vNotInformed: Boolean = True;

procedure DebugCallBack(Source: GLenum; type_: GLenum; id: GLuint;
  severity: GLenum; length: GLSizei; const message: PGLChar; userParam: Pointer);
{$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
begin
  {$IFDEF USE_LOGGING}
   if length > 0 then
      ShowMwssage(string(message));
  {$ENDIF}
end;

procedure DebugCallBackAMD(id: GLuint; category: GLenum; severity: GLenum;
  length: GLSizei; message: PGLChar; userParam: Pointer);
{$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
begin
  if length > 0 then
    ShowMessage(string(message));
end;
procedure ClearOpenGLError;
var
  n: integer;
begin
  n := 0;
  while (glGetError <> GL_NO_ERROR) and (n < 6) do
    Inc(n);
end;


procedure CheckOpenGLError;
var
  glError: GLuint;
  Count: word;
begin
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
      if not (GL_ARB_debug_output) then
        case glError of
          GL_INVALID_ENUM: ShowMessage('Invalid enum');
          GL_INVALID_VALUE: ShowMessage('Invalid value');
          GL_INVALID_OPERATION: ShowMessage('Invalid Operation');
          GL_OUT_OF_MEMORY: ShowMessage('Out of memory');
        end;
    end;
  except
    ShowMessage('Exception in glGetError');
  end;
end;

procedure GLCap;
{$IFDEF MSWINDOWS} stdcall;{$ELSE} cdecl;{$ENDIF}
begin
  ShowMessage('Call OpenVX function with undefined entry point');
  Abort;
end;

constructor TVXExtensionsAndEntryPoints.Create;
begin
  FInitialized := False;
end;

function TVXExtensionsAndEntryPoints.GetAddress(ProcName: string): Pointer;
var
  vName: string;
begin
  vName := glPrefix + ProcName;
  Result := GetProcAddressGLS(PGLChar(vName));
  if Result = nil then
  begin
    vName := glPrefix + ProcName + 'ARB';
    Result := GetProcAddressGLS(PGLChar(vName));
    if Result = nil then
    begin
      vName := glPrefix + ProcName;
      Result := GetProcAddressGLLib(PGLChar(vName));
      if Result = nil then
      begin
        vName := glPrefix + ProcName + 'EXT';
        Result := GetProcAddressGLS(PGLChar(vName));
        if Result = nil then
        begin
          vName := glPrefix + ProcName + 'NV';
          Result := GetProcAddressGLS(PGLChar(vName));
          if Result = nil then
          begin
            vName := glPrefix + ProcName + 'ATI';
            Result := GetProcAddressGLS(PGLChar(vName));
            if Result = nil then
            begin
              vName := glPrefix + ProcName + 'OES';
              Result := GetProcAddressGLS(PGLChar(vName));
              if Result = nil then
                Result := @glCap;
            end;
          end;
        end;
      end;
    end;
  end;
 
{$IFDEF USE_OPENGL_DEBUG}
  if Result <> @glCap then
    GLSLogger.LogDebug('Finded entry point of ' + vName)
  else
    GLSLogger.LogDebug('Can''t find entry point of ' + vName);
{$ENDIF}
end;

function TVXExtensionsAndEntryPoints.GetAddressAlt(ProcName1, ProcName2:
  string): Pointer;
begin
  Result := GetAddress(ProcName1);
  if Result = @glCap then
    Result := GetAddress(ProcName2);
end;

function TVXExtensionsAndEntryPoints.GetAddressNoSuffixes(ProcName: string): Pointer;
var
  vName: string;
begin
  vName := glPrefix + ProcName;
  Result := GetProcAddressGLS(PGLChar(vName));
  if Result = nil then
    Result := @glCap;
{$IFDEF USE_OPENGL_DEBUG}
  if Result <> @glCap then
    GLSLogger.LogDebug('Finded entry point of ' + vName)
  else
    GLSLogger.LogDebug('Can''t find entry point of ' + vName);
{$ENDIF}
end;

function TVXExtensionsAndEntryPoints.GetCapAddress: Pointer;
begin
  Result := @glCap;
end;


function TVXExtensionsAndEntryPoints.CheckExtension(const Extension: string): Boolean;
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
{$IFDEF USE_OPENGL_DEBUG}
  if Result then
    GLSLogger.LogDebug(Extension);
{$ENDIF}
end;

procedure TVXExtensionsAndEntryPoints.Initialize(ATemporary: boolean);
var
  i: integer;
  numExt: GLint;
  MajorVersion, MinorVersion: integer;
  name: GLenum;
  pname: GLenum;
  params: PGLint;
begin
  ShowMessage('Getting OpenGL entry points and extension');
  {$IFDEF SUPPORT_WGL}
  ReadWGLExtensions;
  ReadWGLImplementationProperties;
  {$ENDIF}
  {$IFDEF SUPPORT_GLX}
  ReadGLXExtensions;
  ReadGLXImplementationProperties;
 {$ENDIF}
  {$IFDEF EGL_SUPPORT}
  ReadEGLExtensions;
  ReadEGLImplementationProperties;
  {$ENDIF}
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
    ShowMessage('');
    ShowMessage('OpenVX rendering context information:');
    ShowMessage(format('Renderer     : %s', [glGetString(GL_RENDERER)]));
    ShowMessage(format('Vendor       : %s', [glGetString(GL_VENDOR)]));
    ShowMessage(format('Version      : %s', [glGetString(GL_VERSION)]));
    if GL_VERSION_2_0 then
      ShowMessage(format('GLSL version : %s', [glGetString(GL_SHADING_LANGUAGE_VERSION)]))
    else
      ShowMessage('GLSL version : not supported');
    ShowMessage('');
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

  

  if FDebug then
    if GL_ARB_debug_output then
    begin
      glDebugMessageCallback(DebugCallBack, nil);
      glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, FDebugIds, 1);
      glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
    end
    else if GL_AMDX_debug_output then
    begin
      glDebugMessageCallbackAMD(nil, nil); ///glDebugMessageCallbackAMD(DebugCallBackAMD, nil);
      glDebugMessageEnableAMD(0, 0, 0, FDebugIds, 1);
    end
    else
      FDebug := False;

  SetLength(FBuffer, 0);
  FInitialized := True;
end;

procedure TVXExtensionsAndEntryPoints.Close;
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
      glDebugMessageCallbackAMD(nil, nil);
      glDebugMessageEnableAMD(0, 0, 0, FDebugIds, 0);
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

// ReadWGLExtensions
//

procedure TVXExtensionsAndEntryPoints.ReadWGLExtensions;
begin
  // ARB wgl extensions

  // ###########################################################
  // locating functions and procedures for
  // ARB approved WGL extensions
  // ###########################################################

  // WGL_buffer_region (ARB #4)
  wglCreateBufferRegionARB := GetProcAddressGLS('wglCreateBufferRegionARB');
  wglDeleteBufferRegionARB := GetProcAddressGLS('wglDeleteBufferRegionARB');
  wglSaveBufferRegionARB := GetProcAddressGLS('wglSaveBufferRegionARB');
  wglRestoreBufferRegionARB := GetProcAddressGLS('wglRestoreBufferRegionARB');

  // WGL_ARB_extensions_string (ARB #8)
  wglGetExtensionsStringARB := GetProcAddressGLS('wglGetExtensionsStringARB');

  // WGL_ARB_pixel_format (ARB #9)
  wglGetPixelFormatAttribivARB := GetProcAddressGLS('wglGetPixelFormatAttribivARB');
  wglGetPixelFormatAttribfvARB := GetProcAddressGLS('wglGetPixelFormatAttribfvARB');
  wglChoosePixelFormatARB := GetProcAddressGLS('wglChoosePixelFormatARB');

  // WGL_make_current_read (ARB #10)
  wglMakeContextCurrentARB := GetProcAddressGLS('wglMakeContextCurrentARB');
  wglGetCurrentReadDCARB := GetProcAddressGLS('wglGetCurrentReadDCARB');

  // WGL_ARB_pbuffer (ARB #11)
  wglCreatePbufferARB := GetProcAddressGLS('wglCreatePbufferARB');
  wglGetPbufferDCARB := GetProcAddressGLS('wglGetPbufferDCARB');
  wglReleasePbufferDCARB := GetProcAddressGLS('wglReleasePbufferDCARB');
  wglDestroyPbufferARB := GetProcAddressGLS('wglDestroyPbufferARB');
  wglQueryPbufferARB := GetProcAddressGLS('wglQueryPbufferARB');

  // WGL_ARB_render_texture (ARB #20)
  wglBindTexImageARB := GetProcAddressGLS('wglBindTexImageARB');
  wglReleaseTexImageARB := GetProcAddressGLS('wglReleaseTexImageARB');
  wglSetPbufferAttribARB := GetProcAddressGLS('wglSetPbufferAttribARB');

  // WGL_ARB_create_context (ARB #55)
  wglCreateContextAttribsARB := GetProcAddressGLS('wglCreateContextAttribsARB');

  // ###########################################################
  // locating functions and procedures for
  // Vendor/EXT WGL extensions
  // ###########################################################

  // WGL_EXT_swap_control (EXT #172)
  wglSwapIntervalEXT := GetProcAddressGLS('wglSwapIntervalEXT');
  wglGetSwapIntervalEXT := GetProcAddressGLS('wglGetSwapIntervalEXT');

  // GL_NV_vertex_array_range (EXT #190)
  wglAllocateMemoryNV := GetProcAddressGLS('wglAllocateMemoryNV');
  wglFreeMemoryNV := GetProcAddressGLS('wglFreeMemoryNV');

  // WGL_NV_gpu_affinity
  wglEnumGpusNV := GetProcAddressGLS('wglEnumGpusNV');
  wglEnumGpuDevicesNV := GetProcAddressGLS('wglEnumGpuDevicesNV');
  wglCreateAffinityDCNV := GetProcAddressGLS('wglCreateAffinityDCNV');
  wglEnumGpusFromAffinityDCNV := GetProcAddressGLS('wglEnumGpusFromAffinityDCNV');
  wglDeleteDCNV := GetProcAddressGLS('wglDeleteDCNV');

  // WGL_NV_DX_interop
  wglDXSetResourceShareHandleNV := GetProcAddressGLS('wglDXSetResourceShareHandleNV');
  wglDXOpenDeviceNV := GetProcAddressGLS('wglDXOpenDeviceNV');
  wglDXCloseDeviceNV := GetProcAddressGLS('wglDXCloseDeviceNV');
  wglDXRegisterObjectNV := GetProcAddressGLS('wglDXRegisterObjectNV');
  wglDXUnregisterObjectNV := GetProcAddressGLS('wglDXUnregisterObjectNV');
  wglDXObjectAccessNV := GetProcAddressGLS('wglDXObjectAccessNV');
  wglDXLockObjectsNV := GetProcAddressGLS('wglDXLockObjectsNV');
  wglDXUnlockObjectsNV := GetProcAddressGLS('wglDXUnlockObjectsNV');
end;
{$ENDIF}

// ReadWGLImplementationProperties
//

procedure TVXExtensionsAndEntryPoints.ReadWGLImplementationProperties;
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


{$IFDEF SUPPORT_GLX}
// ReadGLXImplementationProperties
//

procedure TVXExtensionsAndEntryPoints.ReadGLXImplementationProperties;
var
  MajorVersion, MinorVersion: integer;
  dpy: PDisplay;
begin
  dpy := glXGetCurrentDisplay();
  FBuffer := string(glXQueryServerString(dpy, XDefaultScreen(dpy), GLX_VERSION));
  TrimAndSplitVersionString(FBuffer, MajorVersion, MinorVersion);
  GLX_VERSION_1_1 := IsVersionMet(1, 1, MajorVersion, MinorVersion);
  GLX_VERSION_1_2 := IsVersionMet(1, 2, MajorVersion, MinorVersion);
  GLX_VERSION_1_3 := IsVersionMet(1, 3, MajorVersion, MinorVersion);
  GLX_VERSION_1_4 := IsVersionMet(1, 4, MajorVersion, MinorVersion);

  // This procedure will probably need changing, as totally untested
  // This might only work if GLX functions/procedures are loaded dynamically
  if Assigned(glXQueryExtensionsString) then
    FBuffer := glXQueryExtensionsString(dpy, 0) // guess at a valid screen
  else
    FBuffer := '';
  GLX_ARB_create_context := CheckExtension('GLX_ARB_create_context');
  GLX_ARB_create_context_profile := CheckExtension('GLX_ARB_create_context_profile');
  GLX_ARB_framebuffer_sRGB := CheckExtension('GLX_ARB_framebuffer_sRGB');
  GLX_EXT_framebuffer_sRGB := CheckExtension('GLX_EXT_framebuffer_sRGB');
  GLX_EXT_fbconfig_packed_float := CheckExtension('GLX_EXT_fbconfig_packed_float');
  GLX_SGI_swap_control := CheckExtension('GLX_SGI_swap_control');
  GLX_ARB_multisample := CheckExtension('GLX_ARB_multisample');

  GLX_SGIS_multisample := CheckExtension('GLX_SGIS_multisample');
  GLX_EXT_visual_info := CheckExtension('GLX_EXT_visual_info');
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
  GLX_SGIS_shared_multisample := CheckExtension('GLX_SGIS_shared_multisample');
  GLX_SUN_get_transparent_index := CheckExtension('GLX_SUN_get_transparent_index');
  GLX_3DFX_multisample := CheckExtension('GLX_3DFX_multisample');
  GLX_MESA_copy_sub_buffer := CheckExtension('GLX_MESA_copy_sub_buffer');
  GLX_MESA_pixmap_colormap := CheckExtension('GLX_MESA_pixmap_colormap');
  GLX_MESA_release_buffers := CheckExtension('GLX_MESA_release_buffers');
  GLX_MESA_set_3dfx_mode := CheckExtension('GLX_MESA_set_3dfx_mode');
  GLX_SGIX_visual_select_group := CheckExtension('GLX_SGIX_visual_select_group');
  GLX_SGIX_hyperpipe := CheckExtension('GLX_SGIX_hyperpipe');
  GLX_NV_multisample_coverage := CheckExtension('GLX_NV_multisample_coverage');
end;

// ReadGLXExtensions

procedure TVXExtensionsAndEntryPoints.ReadGLXExtensions;
begin
  // ARB glx extensions

  // ###########################################################
  // locating functions and procedures for
  // ARB approved GLX extensions
  // ###########################################################

  // GLX 1.3 and later
  glXChooseFBConfig := GetProcAddressGLS('glXChooseFBConfig');
  glXGetFBConfigAttrib := GetProcAddressGLS('glXGetFBConfigAttrib');
  glXGetFBConfigs := GetProcAddressGLS('glXGetFBConfigs');
  glXGetVisualFromFBConfig := GetProcAddressGLS('glXGetVisualFromFBConfig');
  glXCreateWindow := GetProcAddressGLS('glXCreateWindow');
  glXDestroyWindow := GetProcAddressGLS('glXDestroyWindow');
  glXCreatePixmap := GetProcAddressGLS('glXCreatePixmap');
  glXDestroyPixmap := GetProcAddressGLS('glXDestroyPixmap');
  glXCreatePbuffer := GetProcAddressGLS('glXCreatePbuffer');
  glXDestroyPbuffer := GetProcAddressGLS('glXDestroyPbuffer');
  glXQueryDrawable := GetProcAddressGLS('glXQueryDrawable');
  glXCreateNewContext := GetProcAddressGLS('glXCreateNewContext');
  glXMakeContextCurrent := GetProcAddressGLS('glXMakeContextCurrent');
  glXGetCurrentReadDrawable := GetProcAddressGLS('glXGetCurrentReadDrawable');
  glXQueryContext := GetProcAddressGLS('glXQueryContext');
  glXSelectEvent := GetProcAddressGLS('glXSelectEvent');
  glXGetSelectedEvent := GetProcAddressGLS('glXGetSelectedEvent');
  glXBindTexImageARB := GetProcAddressGLS('glXBindTexImageARB');
  glXReleaseTexImageARB := GetProcAddressGLS('glXReleaseTexImageARB');
  glXDrawableAttribARB := GetProcAddressGLS('glxDrawableAttribARB');

  // GLX 1.4
  // GLX_ARB_create_context (EXT #56)
  glXCreateContextAttribsARB := GetProcAddressGLS('glXCreateContextAttribsARB');

  // ###########################################################
  // locating functions and procedures for
  // Vendor/EXT WGL extensions
  // ###########################################################

  // WGL_EXT_swap_control (EXT #172)
  glXSwapIntervalSGI := GetProcAddressGLS('glXSwapIntervalSGI');
  glXGetVideoSyncSGI := GetProcAddressGLS('glXGetVideoSyncSGI');
  glXWaitVideoSyncSGI := GetProcAddressGLS('glXWaitVideoSyncSGI');
  glXFreeContextEXT := GetProcAddressGLS('glXFreeContextEXT');
  glXGetContextIDEXT := GetProcAddressGLS('glXGetContextIDEXT');
  glXGetCurrentDisplayEXT := GetProcAddressGLS('glXGetCurrentDisplayEXT');
  glXImportContextEXT := GetProcAddressGLS('glXImportContextEXT');
  glXQueryContextInfoEXT := GetProcAddressGLS('glXQueryContextInfoEXT');
  glXCopySubBufferMESA := GetProcAddressGLS('glXCopySubBufferMESA');
  glXCreateGLXPixmapMESA := GetProcAddressGLS('glXCreateGLXPixmapMESA');
  glXReleaseBuffersMESA := GetProcAddressGLS('glXReleaseBuffersMESA');
  glXSet3DfxModeMESA := GetProcAddressGLS('glXSet3DfxModeMESA');

  glXBindTexImageEXT := GetProcAddressGLS('glXBindTexImageEXT');
  glXReleaseTexImageEXT := GetProcAddressGLS('glXReleaseTexImageEXT');

  // GLX 1.4
  glXMakeCurrentReadSGI := GetProcAddressGLS('glXMakeCurrentReadSGI');
  glXGetCurrentReadDrawableSGI := GetProcAddressGLS('glXGetCurrentReadDrawableSGI');
  glXGetFBConfigAttribSGIX := GetProcAddressGLS('glXGetFBConfigAttribSGIX');
  glXChooseFBConfigSGIX := GetProcAddressGLS('glXChooseFBConfigSGIX');
  glXCreateGLXPixmapWithConfigSGIX := GetProcAddressGLS('glXCreateGLXPixmapWithConfigSGIX');
  glXCreateContextWithConfigSGIX := GetProcAddressGLS('glXCreateContextWithConfigSGIX');
  glXGetVisualFromFBConfigSGIX := GetProcAddressGLS('glXGetVisualFromFBConfigSGIX');
  glXGetFBConfigFromVisualSGIX := GetProcAddressGLS('glXGetFBConfigFromVisualSGIX');
  glXCreateGLXPbufferSGIX := GetProcAddressGLS('glXCreateGLXPbufferSGIX');
  glXDestroyGLXPbufferSGIX := GetProcAddressGLS('glXDestroyGLXPbufferSGIX');
  glXQueryGLXPbufferSGIX := GetProcAddressGLS('glXQueryGLXPbufferSGIX');
  glXSelectEventSGIX := GetProcAddressGLS('glXSelectEventSGIX');
  glXGetSelectedEventSGIX := GetProcAddressGLS('glXGetSelectedEventSGIX');
  glXCushionSGI := GetProcAddressGLS('glXCushionSGI');
  glXBindChannelToWindowSGIX := GetProcAddressGLS('glXBindChannelToWindowSGIX');
  glXChannelRectSGIX := GetProcAddressGLS('glXChannelRectSGIX');
  glXQueryChannelRectSGIX := GetProcAddressGLS('glXQueryChannelRectSGIX');
  glXQueryChannelDeltasSGIX := GetProcAddressGLS('glXQueryChannelDeltasSGIX');
  glXChannelRectSyncSGIX := GetProcAddressGLS('glXChannelRectSyncSGIX');
  glXJoinSwapGroupSGIX := GetProcAddressGLS('glXJoinSwapGroupSGIX');
  glXBindSwapBarrierSGIX := GetProcAddressGLS('glXBindSwapBarrierSGIX');
  glXQueryMaxSwapBarriersSGIX := GetProcAddressGLS('glXQueryMaxSwapBarriersSGIX');
  glXQueryHyperpipeNetworkSGIX := GetProcAddressGLS('glXQueryHyperpipeNetworkSGIX');

  glXHyperpipeConfigSGIX := GetProcAddressGLS('glXHyperpipeConfigSGIX');
  glXQueryHyperpipeConfigSGIX := GetProcAddressGLS('glXQueryHyperpipeConfigSGIX');
  glXDestroyHyperpipeConfigSGIX := GetProcAddressGLS('glXDestroyHyperpipeConfigSGIX');
  glXBindHyperpipeSGIX := GetProcAddressGLS('glXBindHyperpipeSGIX');
  glXQueryHyperpipeBestAttribSGIX := GetProcAddressGLS('glXQueryHyperpipeBestAttribSGIX');
  glXHyperpipeAttribSGIX := GetProcAddressGLS('glXHyperpipeAttribSGIX');
  glXQueryHyperpipeAttribSGIX := GetProcAddressGLS('glXQueryHyperpipeAttribSGIX');
  glXGetAGPOffsetMESA := GetProcAddressGLS('glXGetAGPOffsetMESA');
  glXEnumerateVideoDevicesNV := GetProcAddressGLS('glXEnumerateVideoDevicesNV');
  glXBindVideoDeviceNV := GetProcAddressGLS('glXBindVideoDeviceNV');
  glXGetVideoDeviceNV := GetProcAddressGLS('glXGetVideoDeviceNV');
  glXCopySubBufferMESA := GetProcAddressGLS('glXCopySubBufferMESA');
  glXReleaseBuffersMESA := GetProcAddressGLS('glXReleaseBuffersMESA');
  glXCreateGLXPixmapMESA := GetProcAddressGLS('glXCreateGLXPixmapMESA');
  glXSet3DfxModeMESA := GetProcAddressGLS('glXSet3DfxModeMESA');

  glXAllocateMemoryNV := GetProcAddressGLS('glXAllocateMemoryNV');
  glXFreeMemoryNV := GetProcAddressGLS('glXFreeMemoryNV');

  glXReleaseVideoDeviceNV := GetProcAddressGLS('glXReleaseVideoDeviceNV');
  glXBindVideoImageNV := GetProcAddressGLS('glXBindVideoImageNV');
  glXReleaseVideoImageNV := GetProcAddressGLS('glXReleaseVideoImageNV');
  glXSendPbufferToVideoNV := GetProcAddressGLS('glXSendPbufferToVideoNV');
  glXGetVideoInfoNV := GetProcAddressGLS('glXGetVideoInfoNV');
  glXJoinSwapGroupNV := GetProcAddressGLS('glXJoinSwapGroupNV');
  glXBindSwapBarrierNV := GetProcAddressGLS('glXBindSwapBarrierNV');
  glXQuerySwapGroupNV := GetProcAddressGLS('glXQuerySwapGroupNV');
  glXQueryMaxSwapGroupsNV := GetProcAddressGLS('glXQueryMaxSwapGroupsNV');
  glXQueryFrameCountNV := GetProcAddressGLS('glXQueryFrameCountNV');
  glXResetFrameCountNV := GetProcAddressGLS('glXResetFrameCountNV');
  glXBindVideoCaptureDeviceNV := GetProcAddressGLS('glXBindVideoCaptureDeviceNV');
  glXEnumerateVideoCaptureDevicesNV :=
    GetProcAddressGLS('glXEnumerateVideoCaptureDevicesNV');
  glXLockVideoCaptureDeviceNV := GetProcAddressGLS('glxLockVideoCaptureDeviceNV');
  glXQueryVideoCaptureDeviceNV := GetProcAddressGLS('glXQueryVideoCaptureDeviceNV');
  glXReleaseVideoCaptureDeviceNV := GetProcAddressGLS('glXReleaseVideoCaptureDeviceNV');
  glXSwapIntervalEXT := GetProcAddressGLS('glXSwapIntervalEXT');
  glXCopyImageSubDataNV := GetProcAddressGLS('glXCopyImageSubDataNV');
end;

{$ENDIF}

{$IFDEF DARWIN}
// ReadAGLImplementationProperties
//

procedure TVXExtensionsAndEntryPoints.ReadAGLImplementationProperties;
var
  MajorVersion, MinorVersion: integer;
begin
  // This procedure will probably need changing, as totally untested
  // This might only work if AGL functions/procedures are loaded dynamically
  if Assigned(glGetString) then
    FBuffer := string(glGetString(GL_EXTENSIONS))
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

procedure TVXExtensionsAndEntryPoints.ReadAGLExtensions;
begin
  // Managing pixel format object
  aglCreatePixelFormat := GetProcAddressAGL('aglCreatePixelFormat');
  aglChoosePixelFormat := GetProcAddressAGL('aglChoosePixelFormat');
  aglDestroyPixelFormat := GetProcAddressAGL('aglDestroyPixelFormat');
  aglDescribePixelFormat := GetProcAddressAGL('aglDescribePixelFormat');
  aglDestroyPixelFormat := GetProcAddressAGL('aglDestroyPixelFormat');
  aglGetCGLPixelFormat := GetProcAddressAGL('aglGetCGLPixelFormat');
  aglDisplaysOfPixelFormat := GetProcAddressAGL('aglDisplaysOfPixelFormat');
  aglNextPixelFormat := GetProcAddressAGL('aglNextPixelFormat');
  // Managing context
  aglCreateContext := GetProcAddressAGL('aglCreateContext');
  aglCopyContext := GetProcAddressAGL('aglCopyContext');
  aglDestroyContext := GetProcAddressAGL('aglDestroyContext');
  aglUpdateContext := GetProcAddressAGL('aglUpdateContext');
  aglSetCurrentContext := GetProcAddressAGL('aglSetCurrentContext');
  aglGetCGLContext := GetProcAddressAGL('aglGetCGLContext');
  aglGetCurrentContext := GetProcAddressAGL('aglGetCurrentContext');
  aglSwapBuffers := GetProcAddressAGL('aglSwapBuffers');
  aglUpdateContext := GetProcAddressAGL('aglUpdateContext');
  // Managing Pixel Buffers
  aglCreatePBuffer := GetProcAddressAGL('aglCreatePBuffer');
  aglDestroyPBuffer := GetProcAddressAGL('aglDestroyPBuffer');
  aglDescribePBuffer := GetProcAddressAGL('aglDescribePBuffer');
  aglGetPBuffer := GetProcAddressAGL('aglGetPBuffer');
  aglSetPBuffer := GetProcAddressAGL('aglSetPBuffer');
  aglTexImagePBuffer := GetProcAddressAGL('aglTexImagePBuffer');
  // Managing Drawable Objects
  aglSetDrawable := GetProcAddressAGL('aglSetDrawable'); // deprecated
  aglGetDrawable := GetProcAddressAGL('aglGetDrawable'); // deprecated
  aglSetFullScreen := GetProcAddressAGL('aglSetFullScreen');
  aglSetOffScreen := GetProcAddressAGL('aglSetOffScreen');
  // Getting and Setting Context Options
  aglEnable := GetProcAddressAGL('aglEnable');
  aglDisable := GetProcAddressAGL('aglDisable');
  aglIsEnabled := GetProcAddressAGL('aglIsEnabled');
  aglSetInteger := GetProcAddressAGL('aglSetInteger');
  aglGetInteger := GetProcAddressAGL('aglGetInteger');
  // Getting and Setting Global Information
  aglConfigure := GetProcAddressAGL('aglConfigure');
  aglGetVersion := GetProcAddressAGL('aglGetVersion');
  aglResetLibrary := GetProcAddressAGL('aglResetLibrary');
  // Getting Renderer Information
  aglDescribeRenderer := GetProcAddressAGL('aglDescribeRenderer');
  aglDestroyRendererInfo := GetProcAddressAGL('aglDestroyRendererInfo');
  aglNextRendererInfo := GetProcAddressAGL('aglNextRendererInfo');
  aglQueryRendererInfoForCGDirectDisplayIDs := GetProcAddressAGL('aglQueryRendererInfoForCGDirectDisplayIDs');
  // Managing Virtual Screens
  aglGetVirtualScreen := GetProcAddressAGL('aglGetVirtualScreen');
  aglSetVirtualScreen := GetProcAddressAGL('aglSetVirtualScreen');
  // Getting and Setting Windows
  aglSetWindowRef := GetProcAddressAGL('aglSetWindowRef');
  aglGetWindowRef := GetProcAddressAGL('aglGetWindowRef');
  // Getting and Setting HIView Objects
  aglSetHIViewRef := GetProcAddressAGL('aglSetHIViewRef');
  aglGetHIViewRef := GetProcAddressAGL('aglGetHIViewRef');
  // Getting Error Information
  aglGetError := GetProcAddressAGL('aglGetError');
  aglErrorString := GetProcAddressAGL('aglErrorString');
end;
{$ENDIF}

{$IFDEF EGL_SUPPORT}
procedure TVXExtensionsAndEntryPoints.ReadEGLImplementationProperties;
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

procedure TVXExtensionsAndEntryPoints.ReadEGLExtensions;
begin
  eglGetError := EGLGetProcAddress('eglGetError');
  eglGetDisplay := EGLGetProcAddress('eglGetDisplay');
  eglInitialize := EGLGetProcAddress('eglInitialize');
  eglTerminate := EGLGetProcAddress('eglTerminate');
  eglQueryString := EGLGetProcAddress('eglQueryString');
  eglGetConfigs := EGLGetProcAddress('eglGetConfigs');
  eglChooseConfig := EGLGetProcAddress('eglChooseConfig');
  eglGetConfigAttrib := EGLGetProcAddress('eglGetConfigAttrib');
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
function InitOpenGL: Boolean;
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

///  InitOpenGLext;  //< Winapi.OpenGLext

end;

// InitOpenGLFromLibrary
//
function InitOpenGLFromLibrary(const GLName, GLUName : String) : Boolean;
begin
  Result := False;
  CloseOpenGL;

  GLHandle := LoadLibrary(PChar(GLName));
  GLUHandle := LoadLibrary(PChar(GLUName));

{$IFDEF Linux}   // make it work when mesa-dev is not installed and only libGL.so.1 is available
  if (GLHandle=INVALID_MODULEHANDLE) then
         GLHandle := LoadLibrary(PChar(GLName+'.1'));
  if (GLUHandle = INVALID_MODULEHANDLE) then
         GLUHandle := LoadLibrary(PChar(GLUName+'.1'));
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
  Result := GetProcAddressGLS('glResizeBuffersMESA') <> nil;
end;

initialization

  Set8087CW($133F);

finalization

  CloseOpenGL;

end.