
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'OpenGL 1.1 core functions and procedures'}{$ENDIF}
procedure glCapBindTexture();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindTexture')
end;
procedure glCapBlendFunc();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BlendFunc')
end;
procedure glCapClear();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Clear')
end;
procedure glCapClearColor();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClearColor')
end;
procedure glCapClearDepth();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClearDepth')
end;
procedure glCapClearStencil();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClearStencil')
end;
procedure glCapColorMask();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ColorMask')
end;
procedure glCapCopyTexImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyTexImage1D')
end;
procedure glCapCopyTexImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyTexImage2D')
end;
procedure glCapCopyTexSubImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyTexSubImage1D')
end;
procedure glCapCopyTexSubImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyTexSubImage2D')
end;
procedure glCapCullFace();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CullFace')
end;
procedure glCapDeleteTextures();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteTextures')
end;
procedure glCapDepthFunc();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DepthFunc')
end;
procedure glCapDepthMask();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DepthMask')
end;
procedure glCapDepthRange();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DepthRange')
end;
procedure glCapDisable();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Disable')
end;
procedure glCapDrawArrays();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DrawArrays')
end;
procedure glCapDrawBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DrawBuffer')
end;
procedure glCapDrawElements();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DrawElements')
end;
procedure glCapEnable();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Enable')
end;
procedure glCapFinish();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Finish')
end;
procedure glCapFlush();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Flush')
end;
procedure glCapFrontFace();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FrontFace')
end;
procedure glCapGenTextures();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenTextures')
end;
procedure glCapGetBooleanv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetBooleanv')
end;
procedure glCapGetDoublev();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetDoublev')
end;
procedure glCapGetError();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetError')
end;
procedure glCapGetFloatv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetFloatv')
end;
procedure glCapGetIntegerv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetIntegerv')
end;
procedure glCapGetPointerv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPointerv')
end;
procedure glCapGetString();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetString')
end;
procedure glCapGetTexImage();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexImage')
end;
procedure glCapGetTexLevelParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexLevelParameterfv')
end;
procedure glCapGetTexLevelParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexLevelParameteriv')
end;
procedure glCapGetTexParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexParameterfv')
end;
procedure glCapGetTexParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexParameteriv')
end;
procedure glCapHint();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Hint')
end;
procedure glCapIsEnabled();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsEnabled')
end;
procedure glCapIsTexture();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsTexture')
end;
procedure glCapLineWidth();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LineWidth')
end;
procedure glCapLogicOp();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LogicOp')
end;
procedure glCapPixelStoref();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PixelStoref')
end;
procedure glCapPixelStorei();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PixelStorei')
end;
procedure glCapPointSize();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PointSize')
end;
procedure glCapPolygonMode();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PolygonMode')
end;
procedure glCapPolygonOffset();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PolygonOffset')
end;
procedure glCapReadBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ReadBuffer')
end;
procedure glCapReadPixels();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ReadPixels')
end;
procedure glCapScissor();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Scissor')
end;
procedure glCapStencilFunc();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('StencilFunc')
end;
procedure glCapStencilMask();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('StencilMask')
end;
procedure glCapStencilOp();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('StencilOp')
end;
procedure glCapTexImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexImage1D')
end;
procedure glCapTexImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexImage2D')
end;
procedure glCapTexParameterf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexParameterf')
end;
procedure glCapTexParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexParameterfv')
end;
procedure glCapTexParameteri();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexParameteri')
end;
procedure glCapTexParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexParameteriv')
end;
procedure glCapTexSubImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexSubImage1D')
end;
procedure glCapTexSubImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexSubImage2D')
end;
procedure glCapViewport();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Viewport')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'OpenGL 1.1 deprecated'}{$ENDIF}
procedure glCapAccum();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Accum')
end;
procedure glCapAlphaFunc();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AlphaFunc')
end;
procedure glCapAreTexturesResident();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AreTexturesResident')
end;
procedure glCapArrayElement();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ArrayElement')
end;
procedure glCapBegin_();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Begin_')
end;
procedure glCapBitmap();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Bitmap')
end;
procedure glCapCallList();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CallList')
end;
procedure glCapCallLists();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CallLists')
end;
procedure glCapClearAccum();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClearAccum')
end;
procedure glCapClearIndex();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClearIndex')
end;
procedure glCapClipPlane();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClipPlane')
end;
procedure glCapColor3b();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3b')
end;
procedure glCapColor3bv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3bv')
end;
procedure glCapColor3d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3d')
end;
procedure glCapColor3dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3dv')
end;
procedure glCapColor3f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3f')
end;
procedure glCapColor3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3fv')
end;
procedure glCapColor3i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3i')
end;
procedure glCapColor3iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3iv')
end;
procedure glCapColor3s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3s')
end;
procedure glCapColor3sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3sv')
end;
procedure glCapColor3ub();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3ub')
end;
procedure glCapColor3ubv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3ubv')
end;
procedure glCapColor3ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3ui')
end;
procedure glCapColor3uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3uiv')
end;
procedure glCapColor3us();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3us')
end;
procedure glCapColor3usv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color3usv')
end;
procedure glCapColor4b();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4b')
end;
procedure glCapColor4bv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4bv')
end;
procedure glCapColor4d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4d')
end;
procedure glCapColor4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4dv')
end;
procedure glCapColor4f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4f')
end;
procedure glCapColor4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4fv')
end;
procedure glCapColor4i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4i')
end;
procedure glCapColor4iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4iv')
end;
procedure glCapColor4s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4s')
end;
procedure glCapColor4sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4sv')
end;
procedure glCapColor4ub();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4ub')
end;
procedure glCapColor4ubv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4ubv')
end;
procedure glCapColor4ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4ui')
end;
procedure glCapColor4uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4uiv')
end;
procedure glCapColor4us();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4us')
end;
procedure glCapColor4usv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Color4usv')
end;
procedure glCapColorMaterial();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ColorMaterial')
end;
procedure glCapColorPointer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ColorPointer')
end;
procedure glCapCopyPixels();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyPixels')
end;
procedure glCapDeleteLists();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteLists')
end;
procedure glCapDisableClientState();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DisableClientState')
end;
procedure glCapDrawPixels();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DrawPixels')
end;
procedure glCapEdgeFlag();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EdgeFlag')
end;
procedure glCapEdgeFlagPointer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EdgeFlagPointer')
end;
procedure glCapEdgeFlagv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EdgeFlagv')
end;
procedure glCapEnableClientState();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EnableClientState')
end;
procedure glCapEnd_();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('End_')
end;
procedure glCapEndList();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EndList')
end;
procedure glCapEvalCoord1d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalCoord1d')
end;
procedure glCapEvalCoord1dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalCoord1dv')
end;
procedure glCapEvalCoord1f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalCoord1f')
end;
procedure glCapEvalCoord1fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalCoord1fv')
end;
procedure glCapEvalCoord2d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalCoord2d')
end;
procedure glCapEvalCoord2dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalCoord2dv')
end;
procedure glCapEvalCoord2f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalCoord2f')
end;
procedure glCapEvalCoord2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalCoord2fv')
end;
procedure glCapEvalMesh1();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalMesh1')
end;
procedure glCapEvalMesh2();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalMesh2')
end;
procedure glCapEvalPoint1();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalPoint1')
end;
procedure glCapEvalPoint2();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EvalPoint2')
end;
procedure glCapFeedbackBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FeedbackBuffer')
end;
procedure glCapFogf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Fogf')
end;
procedure glCapFogfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Fogfv')
end;
procedure glCapFogi();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Fogi')
end;
procedure glCapFogiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Fogiv')
end;
procedure glCapFrustum();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Frustum')
end;
procedure glCapGenLists();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenLists')
end;
procedure glCapGetClipPlane();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetClipPlane')
end;
procedure glCapGetLightfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetLightfv')
end;
procedure glCapGetLightiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetLightiv')
end;
procedure glCapGetMapdv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMapdv')
end;
procedure glCapGetMapfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMapfv')
end;
procedure glCapGetMapiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMapiv')
end;
procedure glCapGetMaterialfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMaterialfv')
end;
procedure glCapGetMaterialiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMaterialiv')
end;
procedure glCapGetPixelMapfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPixelMapfv')
end;
procedure glCapGetPixelMapuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPixelMapuiv')
end;
procedure glCapGetPixelMapusv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPixelMapusv')
end;
procedure glCapGetPolygonStipple();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPolygonStipple')
end;
procedure glCapGetTexEnvfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexEnvfv')
end;
procedure glCapGetTexEnviv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexEnviv')
end;
procedure glCapGetTexGendv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexGendv')
end;
procedure glCapGetTexGenfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexGenfv')
end;
procedure glCapGetTexGeniv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexGeniv')
end;
procedure glCapIndexMask();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IndexMask')
end;
procedure glCapIndexPointer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IndexPointer')
end;
procedure glCapIndexd();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Indexd')
end;
procedure glCapIndexdv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Indexdv')
end;
procedure glCapIndexf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Indexf')
end;
procedure glCapIndexfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Indexfv')
end;
procedure glCapIndexi();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Indexi')
end;
procedure glCapIndexiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Indexiv')
end;
procedure glCapIndexs();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Indexs')
end;
procedure glCapIndexsv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Indexsv')
end;
procedure glCapIndexub();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Indexub')
end;
procedure glCapIndexubv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Indexubv')
end;
procedure glCapInitNames();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('InitNames')
end;
procedure glCapInterleavedArrays();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('InterleavedArrays')
end;
procedure glCapIsList();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsList')
end;
procedure glCapLightModelf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LightModelf')
end;
procedure glCapLightModelfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LightModelfv')
end;
procedure glCapLightModeli();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LightModeli')
end;
procedure glCapLightModeliv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LightModeliv')
end;
procedure glCapLightf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Lightf')
end;
procedure glCapLightfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Lightfv')
end;
procedure glCapLighti();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Lighti')
end;
procedure glCapLightiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Lightiv')
end;
procedure glCapLineStipple();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LineStipple')
end;
procedure glCapListBase();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ListBase')
end;
procedure glCapLoadIdentity();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LoadIdentity')
end;
procedure glCapLoadMatrixd();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LoadMatrixd')
end;
procedure glCapLoadMatrixf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LoadMatrixf')
end;
procedure glCapLoadName();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LoadName')
end;
procedure glCapMap1d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Map1d')
end;
procedure glCapMap1f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Map1f')
end;
procedure glCapMap2d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Map2d')
end;
procedure glCapMap2f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Map2f')
end;
procedure glCapMapGrid1d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MapGrid1d')
end;
procedure glCapMapGrid1f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MapGrid1f')
end;
procedure glCapMapGrid2d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MapGrid2d')
end;
procedure glCapMapGrid2f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MapGrid2f')
end;
procedure glCapMaterialf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Materialf')
end;
procedure glCapMaterialfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Materialfv')
end;
procedure glCapMateriali();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Materiali')
end;
procedure glCapMaterialiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Materialiv')
end;
procedure glCapMatrixMode();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixMode')
end;
procedure glCapMultMatrixd();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultMatrixd')
end;
procedure glCapMultMatrixf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultMatrixf')
end;
procedure glCapNewList();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NewList')
end;
procedure glCapNormal3b();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Normal3b')
end;
procedure glCapNormal3bv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Normal3bv')
end;
procedure glCapNormal3d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Normal3d')
end;
procedure glCapNormal3dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Normal3dv')
end;
procedure glCapNormal3f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Normal3f')
end;
procedure glCapNormal3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Normal3fv')
end;
procedure glCapNormal3i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Normal3i')
end;
procedure glCapNormal3iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Normal3iv')
end;
procedure glCapNormal3s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Normal3s')
end;
procedure glCapNormal3sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Normal3sv')
end;
procedure glCapNormalPointer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NormalPointer')
end;
procedure glCapOrtho();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Ortho')
end;
procedure glCapPassThrough();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PassThrough')
end;
procedure glCapPixelMapfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PixelMapfv')
end;
procedure glCapPixelMapuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PixelMapuiv')
end;
procedure glCapPixelMapusv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PixelMapusv')
end;
procedure glCapPixelTransferf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PixelTransferf')
end;
procedure glCapPixelTransferi();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PixelTransferi')
end;
procedure glCapPixelZoom();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PixelZoom')
end;
procedure glCapPolygonStipple();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PolygonStipple')
end;
procedure glCapPopAttrib();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PopAttrib')
end;
procedure glCapPopClientAttrib();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PopClientAttrib')
end;
procedure glCapPopMatrix();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PopMatrix')
end;
procedure glCapPopName();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PopName')
end;
procedure glCapPrioritizeTextures();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PrioritizeTextures')
end;
procedure glCapPushAttrib();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PushAttrib')
end;
procedure glCapPushClientAttrib();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PushClientAttrib')
end;
procedure glCapPushMatrix();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PushMatrix')
end;
procedure glCapPushName();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PushName')
end;
procedure glCapRasterPos2d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos2d')
end;
procedure glCapRasterPos2dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos2dv')
end;
procedure glCapRasterPos2f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos2f')
end;
procedure glCapRasterPos2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos2fv')
end;
procedure glCapRasterPos2i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos2i')
end;
procedure glCapRasterPos2iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos2iv')
end;
procedure glCapRasterPos2s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos2s')
end;
procedure glCapRasterPos2sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos2sv')
end;
procedure glCapRasterPos3d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos3d')
end;
procedure glCapRasterPos3dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos3dv')
end;
procedure glCapRasterPos3f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos3f')
end;
procedure glCapRasterPos3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos3fv')
end;
procedure glCapRasterPos3i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos3i')
end;
procedure glCapRasterPos3iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos3iv')
end;
procedure glCapRasterPos3s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos3s')
end;
procedure glCapRasterPos3sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos3sv')
end;
procedure glCapRasterPos4d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos4d')
end;
procedure glCapRasterPos4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos4dv')
end;
procedure glCapRasterPos4f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos4f')
end;
procedure glCapRasterPos4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos4fv')
end;
procedure glCapRasterPos4i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos4i')
end;
procedure glCapRasterPos4iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos4iv')
end;
procedure glCapRasterPos4s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos4s')
end;
procedure glCapRasterPos4sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RasterPos4sv')
end;
procedure glCapRectd();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Rectd')
end;
procedure glCapRectdv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Rectdv')
end;
procedure glCapRectf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Rectf')
end;
procedure glCapRectfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Rectfv')
end;
procedure glCapRecti();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Recti')
end;
procedure glCapRectiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Rectiv')
end;
procedure glCapRects();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Rects')
end;
procedure glCapRectsv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Rectsv')
end;
procedure glCapRenderMode();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RenderMode')
end;
procedure glCapRotated();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Rotated')
end;
procedure glCapRotatef();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Rotatef')
end;
procedure glCapScaled();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Scaled')
end;
procedure glCapScalef();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Scalef')
end;
procedure glCapSelectBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('SelectBuffer')
end;
procedure glCapShadeModel();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ShadeModel')
end;
procedure glCapTexCoord1d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord1d')
end;
procedure glCapTexCoord1dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord1dv')
end;
procedure glCapTexCoord1f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord1f')
end;
procedure glCapTexCoord1fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord1fv')
end;
procedure glCapTexCoord1i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord1i')
end;
procedure glCapTexCoord1iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord1iv')
end;
procedure glCapTexCoord1s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord1s')
end;
procedure glCapTexCoord1sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord1sv')
end;
procedure glCapTexCoord2d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord2d')
end;
procedure glCapTexCoord2dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord2dv')
end;
procedure glCapTexCoord2f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord2f')
end;
procedure glCapTexCoord2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord2fv')
end;
procedure glCapTexCoord2i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord2i')
end;
procedure glCapTexCoord2iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord2iv')
end;
procedure glCapTexCoord2s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord2s')
end;
procedure glCapTexCoord2sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord2sv')
end;
procedure glCapTexCoord3d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord3d')
end;
procedure glCapTexCoord3dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord3dv')
end;
procedure glCapTexCoord3f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord3f')
end;
procedure glCapTexCoord3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord3fv')
end;
procedure glCapTexCoord3i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord3i')
end;
procedure glCapTexCoord3iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord3iv')
end;
procedure glCapTexCoord3s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord3s')
end;
procedure glCapTexCoord3sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord3sv')
end;
procedure glCapTexCoord4d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord4d')
end;
procedure glCapTexCoord4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord4dv')
end;
procedure glCapTexCoord4f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord4f')
end;
procedure glCapTexCoord4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord4fv')
end;
procedure glCapTexCoord4i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord4i')
end;
procedure glCapTexCoord4iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord4iv')
end;
procedure glCapTexCoord4s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord4s')
end;
procedure glCapTexCoord4sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoord4sv')
end;
procedure glCapTexCoordPointer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoordPointer')
end;
procedure glCapTexEnvf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexEnvf')
end;
procedure glCapTexEnvfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexEnvfv')
end;
procedure glCapTexEnvi();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexEnvi')
end;
procedure glCapTexEnviv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexEnviv')
end;
procedure glCapTexGend();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexGend')
end;
procedure glCapTexGendv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexGendv')
end;
procedure glCapTexGenf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexGenf')
end;
procedure glCapTexGenfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexGenfv')
end;
procedure glCapTexGeni();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexGeni')
end;
procedure glCapTexGeniv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexGeniv')
end;
procedure glCapTranslated();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Translated')
end;
procedure glCapTranslatef();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Translatef')
end;
procedure glCapVertex2d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex2d')
end;
procedure glCapVertex2dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex2dv')
end;
procedure glCapVertex2f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex2f')
end;
procedure glCapVertex2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex2fv')
end;
procedure glCapVertex2i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex2i')
end;
procedure glCapVertex2iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex2iv')
end;
procedure glCapVertex2s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex2s')
end;
procedure glCapVertex2sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex2sv')
end;
procedure glCapVertex3d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex3d')
end;
procedure glCapVertex3dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex3dv')
end;
procedure glCapVertex3f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex3f')
end;
procedure glCapVertex3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex3fv')
end;
procedure glCapVertex3i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex3i')
end;
procedure glCapVertex3iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex3iv')
end;
procedure glCapVertex3s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex3s')
end;
procedure glCapVertex3sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex3sv')
end;
procedure glCapVertex4d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex4d')
end;
procedure glCapVertex4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex4dv')
end;
procedure glCapVertex4f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex4f')
end;
procedure glCapVertex4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex4fv')
end;
procedure glCapVertex4i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex4i')
end;
procedure glCapVertex4iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex4iv')
end;
procedure glCapVertex4s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex4s')
end;
procedure glCapVertex4sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Vertex4sv')
end;
procedure glCapVertexPointer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexPointer')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'New core function/procedure definitions in OpenGL 1.2'}
{$ENDIF}
procedure glCapBlendColor();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BlendColor')
end;
procedure glCapBlendEquation();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BlendEquation')
end;
procedure glCapDrawRangeElements();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DrawRangeElements')
end;
procedure glCapTexImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexImage3D')
end;
procedure glCapTexSubImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexSubImage3D')
end;
procedure glCapCopyTexSubImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyTexSubImage3D')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'New core function/procedure definitions in OpenGL 1.4'}
{$ENDIF}
procedure glCapBlendFuncSeparate();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BlendFuncSeparate')
end;
procedure glCapMultiDrawArrays();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiDrawArrays')
end;
procedure glCapMultiDrawElements();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiDrawElements')
end;
procedure glCapPointParameterf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PointParameterf')
end;
procedure glCapPointParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PointParameterfv')
end;
procedure glCapPointParameteri();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PointParameteri')
end;
procedure glCapPointParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PointParameteriv')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'New core function/procedure definitions in OpenGL 2.0'}
{$ENDIF}
procedure glCapBlendEquationSeparate();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BlendEquationSeparate')
end;
procedure glCapDrawBuffers();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DrawBuffers')
end;
procedure glCapStencilOpSeparate();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('StencilOpSeparate')
end;
procedure glCapStencilFuncSeparate();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('StencilFuncSeparate')
end;
procedure glCapStencilMaskSeparate();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('StencilMaskSeparate')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Vertex buffer object'}{$ENDIF}
procedure glCapLockArrays();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LockArrays')
end;
procedure glCapUnlockArrays();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('UnlockArrays')
end;
procedure glCapBindBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindBuffer')
end;
procedure glCapDeleteBuffers();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteBuffers')
end;
procedure glCapGenBuffers();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenBuffers')
end;
procedure glCapIsBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsBuffer')
end;
procedure glCapBufferData();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BufferData')
end;
procedure glCapBufferSubData();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BufferSubData')
end;
procedure glCapGetBufferSubData();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetBufferSubData')
end;
procedure glCapMapBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MapBuffer')
end;
procedure glCapUnmapBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('UnmapBuffer')
end;
procedure glCapGetBufferParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetBufferParameteriv')
end;
procedure glCapGetBufferPointerv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetBufferPointerv')
end;
procedure glCapMapBufferRange();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MapBufferRange')
end;
procedure glCapFlushMappedBufferRange();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FlushMappedBufferRange')
end;
procedure glCapBindBufferRange();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindBufferRange')
end;
procedure glCapBindBufferOffset();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindBufferOffset')
end;
procedure glCapBindBufferBase();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindBufferBase')
end;
procedure glCapTransformFeedbackAttribs();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TransformFeedbackAttribs')
end;
procedure glCapTransformFeedbackVaryingsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TransformFeedbackVaryingsNV')
end;
procedure glCapTransformFeedbackVaryings();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TransformFeedbackVaryings')
end;
procedure glCapGetTransformFeedbackVarying();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTransformFeedbackVarying')
end;
procedure glCapBeginTransformFeedback();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BeginTransformFeedback')
end;
procedure glCapEndTransformFeedback();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EndTransformFeedback')
end;
procedure glCapTexBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexBuffer')
end;
procedure glCapClearBufferiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClearBufferiv')
end;
procedure glCapClearBufferuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClearBufferuiv')
end;
procedure glCapClearBufferfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClearBufferfv')
end;
procedure glCapClearBufferfi();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClearBufferfi')
end;
procedure glCapGetStringi();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetStringi')
end;
procedure glCapBindVertexArray();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindVertexArray')
end;
procedure glCapDeleteVertexArrays();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteVertexArrays')
end;
procedure glCapGenVertexArrays();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenVertexArrays')
end;
procedure glCapIsVertexArray();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsVertexArray')
end;
procedure glCapFlushVertexArrayRangeNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FlushVertexArrayRangeNV')
end;
procedure glCapVertexArrayRangeNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexArrayRangeNV')
end;
procedure glCapGetUniformIndices();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetUniformIndices')
end;
procedure glCapGetActiveUniformsiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetActiveUniformsiv')
end;
procedure glCapGetActiveUniformName();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetActiveUniformName')
end;
procedure glCapGetUniformBlockIndex();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetUniformBlockIndex')
end;
procedure glCapGetActiveUniformBlockiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetActiveUniformBlockiv')
end;
procedure glCapGetActiveUniformBlockName();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetActiveUniformBlockName')
end;
procedure glCapUniformBlockBinding();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('UniformBlockBinding')
end;
procedure glCapCopyBufferSubData();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyBufferSubData')
end;
procedure glCapUniformBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('UniformBuffer')
end;
procedure glCapGetUniformBufferSize();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetUniformBufferSize')
end;
procedure glCapGetUniformOffset();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetUniformOffset')
end;
procedure glCapPrimitiveRestartIndex();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PrimitiveRestartIndex')
end;
procedure glCapDrawElementsBaseVertex();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DrawElementsBaseVertex')
end;
procedure glCapDrawRangeElementsBaseVertex();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DrawRangeElementsBaseVertex')
end;
procedure glCapDrawElementsInstancedBaseVertex();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DrawElementsInstancedBaseVertex')
end;
procedure glCapMultiDrawElementsBaseVertex();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiDrawElementsBaseVertex')
end;
procedure glCapDrawArraysInstanced();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DrawArraysInstanced')
end;
procedure glCapDrawElementsInstanced();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DrawElementsInstanced')
end;
procedure glCapVertexAttrib1d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib1d')
end;
procedure glCapVertexAttrib1dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib1dv')
end;
procedure glCapVertexAttrib1f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib1f')
end;
procedure glCapVertexAttrib1fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib1fv')
end;
procedure glCapVertexAttrib1s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib1s')
end;
procedure glCapVertexAttrib1sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib1sv')
end;
procedure glCapVertexAttrib2d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib2d')
end;
procedure glCapVertexAttrib2dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib2dv')
end;
procedure glCapVertexAttrib2f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib2f')
end;
procedure glCapVertexAttrib2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib2fv')
end;
procedure glCapVertexAttrib2s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib2s')
end;
procedure glCapVertexAttrib2sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib2sv')
end;
procedure glCapVertexAttrib3d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib3d')
end;
procedure glCapVertexAttrib3dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib3dv')
end;
procedure glCapVertexAttrib3f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib3f')
end;
procedure glCapVertexAttrib3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib3fv')
end;
procedure glCapVertexAttrib3s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib3s')
end;
procedure glCapVertexAttrib3sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib3sv')
end;
procedure glCapVertexAttrib4Nbv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4Nbv')
end;
procedure glCapVertexAttrib4Niv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4Niv')
end;
procedure glCapVertexAttrib4Nsv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4Nsv')
end;
procedure glCapVertexAttrib4Nub();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4Nub')
end;
procedure glCapVertexAttrib4Nubv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4Nubv')
end;
procedure glCapVertexAttrib4Nuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4Nuiv')
end;
procedure glCapVertexAttrib4Nusv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4Nusv')
end;
procedure glCapVertexAttrib4bv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4bv')
end;
procedure glCapVertexAttrib4d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4d')
end;
procedure glCapVertexAttrib4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4dv')
end;
procedure glCapVertexAttrib4f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4f')
end;
procedure glCapVertexAttrib4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4fv')
end;
procedure glCapVertexAttrib4iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4iv')
end;
procedure glCapVertexAttrib4s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4s')
end;
procedure glCapVertexAttrib4sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4sv')
end;
procedure glCapVertexAttrib4ubv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4ubv')
end;
procedure glCapVertexAttrib4uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4uiv')
end;
procedure glCapVertexAttrib4usv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttrib4usv')
end;
procedure glCapVertexAttribPointer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribPointer')
end;
procedure glCapVertexAttribI1i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI1i')
end;
procedure glCapVertexAttribI2i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI2i')
end;
procedure glCapVertexAttribI3i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI3i')
end;
procedure glCapVertexAttribI4i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI4i')
end;
procedure glCapVertexAttribI1ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI1ui')
end;
procedure glCapVertexAttribI2ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI2ui')
end;
procedure glCapVertexAttribI3ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI3ui')
end;
procedure glCapVertexAttribI4ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI4ui')
end;
procedure glCapVertexAttribI1iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI1iv')
end;
procedure glCapVertexAttribI2iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI2iv')
end;
procedure glCapVertexAttribI3iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI3iv')
end;
procedure glCapVertexAttribI4iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI4iv')
end;
procedure glCapVertexAttribI1uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI1uiv')
end;
procedure glCapVertexAttribI2uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI2uiv')
end;
procedure glCapVertexAttribI3uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI3uiv')
end;
procedure glCapVertexAttribI4uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI4uiv')
end;
procedure glCapVertexAttribI4bv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI4bv')
end;
procedure glCapVertexAttribI4sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI4sv')
end;
procedure glCapVertexAttribI4ubv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI4ubv')
end;
procedure glCapVertexAttribI4usv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribI4usv')
end;
procedure glCapVertexAttribIPointer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribIPointer')
end;
procedure glCapGetVertexAttribIiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetVertexAttribIiv')
end;
procedure glCapGetVertexAttribIuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetVertexAttribIuiv')
end;
procedure glCapUniform1ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform1ui')
end;
procedure glCapUniform2ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform2ui')
end;
procedure glCapUniform3ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform3ui')
end;
procedure glCapUniform4ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform4ui')
end;
procedure glCapUniform1uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform1uiv')
end;
procedure glCapUniform2uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform2uiv')
end;
procedure glCapUniform3uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform3uiv')
end;
procedure glCapUniform4uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform4uiv')
end;
procedure glCapGetUniformuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetUniformuiv')
end;
procedure glCapBindFragDataLocation();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindFragDataLocation')
end;
procedure glCapGetFragDataLocation();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetFragDataLocation')
end;
procedure glCapClampColor();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClampColor')
end;
procedure glCapColorMaski();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ColorMaski')
end;
procedure glCapGetBooleani_v();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetBooleani_v')
end;
procedure glCapGetIntegeri_v();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetIntegeri_v')
end;
procedure glCapEnablei();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Enablei')
end;
procedure glCapDisablei();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Disablei')
end;
procedure glCapIsEnabledi();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsEnabledi')
end;
procedure glCapEnableVertexAttribArray();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EnableVertexAttribArray')
end;
procedure glCapDisableVertexAttribArray();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DisableVertexAttribArray')
end;
procedure glCapVertexAttribDivisor();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribDivisor')
end;
procedure glCapClearColorIi();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClearColorIi')
end;
procedure glCapClearColorIui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClearColorIui')
end;
procedure glCapTexParameterIiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexParameterIiv')
end;
procedure glCapTexParameterIuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexParameterIuiv')
end;
procedure glCapGetTexParameterIiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexParameterIiv')
end;
procedure glCapGetTexParameterIuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTexParameterIuiv')
end;
procedure glCapPatchParameteri();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PatchParameteri')
end;
procedure glCapPatchParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PatchParameterfv')
end;
procedure glCapBufferAddressRangeNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BufferAddressRangeNV')
end;
procedure glCapVertexFormatNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexFormatNV')
end;
procedure glCapNormalFormatNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NormalFormatNV')
end;
procedure glCapColorFormatNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ColorFormatNV')
end;
procedure glCapIndexFormatNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IndexFormatNV')
end;
procedure glCapTexCoordFormatNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexCoordFormatNV')
end;
procedure glCapEdgeFlagFormatNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EdgeFlagFormatNV')
end;
procedure glCapSecondaryColorFormatNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('SecondaryColorFormatNV')
end;
procedure glCapFogCoordFormatNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FogCoordFormatNV')
end;
procedure glCapVertexAttribFormatNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribFormatNV')
end;
procedure glCapVertexAttribIFormatNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VertexAttribIFormatNV')
end;
procedure glCapGetIntegerui64i_vNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetIntegerui64i_vNV')
end;
procedure glCapGetBufferParameterui64vNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetBufferParameterui64vNV')
end;
procedure glCapMakeBufferResidentNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MakeBufferResidentNV')
end;
procedure glCapMakeBufferNonResidentNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MakeBufferNonResidentNV')
end;
procedure glCapIsBufferResidentNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsBufferResidentNV')
end;
procedure glCapMakeNamedBufferResidentNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MakeNamedBufferResidentNV')
end;
procedure glCapMakeNamedBufferNonResidentNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MakeNamedBufferNonResidentNV')
end;
procedure glCapIsNamedBufferResidentNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsNamedBufferResidentNV')
end;
procedure glCapGetNamedBufferParameterui64vNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedBufferParameterui64vNV')
end;
procedure glCapGetIntegerui64vNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetIntegerui64vNV')
end;
procedure glCapUniformui64NV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniformui64NV')
end;
procedure glCapUniformui64vNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniformui64vNV')
end;
procedure glCapGetUniformui64vNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetUniformui64vNV')
end;
procedure glCapProgramUniformui64NV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformui64NV')
end;
procedure glCapProgramUniformui64vNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformui64vNV')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Shader object'}{$ENDIF}
procedure glCapDeleteObject();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteObject')
end;
{$IFDEF GLS_OPENGL_ES}
procedure glCapDeleteShader();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteShader')
end;
procedure glCapDeleteProgram();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteProgram')
end;
{$ENDIF}
procedure glCapGetHandle();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetHandle')
end;
procedure glCapDetachShader();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DetachShader')
end;
procedure glCapCreateShader();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CreateShader')
end;
procedure glCapShaderSource();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ShaderSource')
end;
procedure glCapCompileShader();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompileShader')
end;
procedure glCapCreateProgram();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CreateProgram')
end;
procedure glCapAttachShader();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AttachShader')
end;
procedure glCapLinkProgram();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('LinkProgram')
end;
procedure glCapUseProgram();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('UseProgram')
end;
procedure glCapValidateProgram();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ValidateProgram')
end;
procedure glCapUniform1f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform1f')
end;
procedure glCapUniform2f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform2f')
end;
procedure glCapUniform3f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform3f')
end;
procedure glCapUniform4f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform4f')
end;
procedure glCapUniform1i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform1i')
end;
procedure glCapUniform2i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform2i')
end;
procedure glCapUniform3i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform3i')
end;
procedure glCapUniform4i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform4i')
end;
procedure glCapUniform1fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform1fv')
end;
procedure glCapUniform2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform2fv')
end;
procedure glCapUniform3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform3fv')
end;
procedure glCapUniform4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform4fv')
end;
procedure glCapUniform1iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform1iv')
end;
procedure glCapUniform2iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform2iv')
end;
procedure glCapUniform3iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform3iv')
end;
procedure glCapUniform4iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('Uniform4iv')
end;
procedure glCapUniformMatrix2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('UniformMatrix2fv')
end;
procedure glCapUniformMatrix3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('UniformMatrix3fv')
end;
procedure glCapUniformMatrix4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('UniformMatrix4fv')
end;
procedure glCapGetObjectParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetObjectParameterfv')
end;
procedure glCapGetObjectParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetObjectParameteriv')
end;
procedure glCapGetInfoLog();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetInfoLog')
end;
procedure glCapGetAttachedObjects();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetAttachedObjects')
end;
procedure glCapGetActiveAttrib();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetActiveAttrib')
end;
procedure glCapGetActiveUniform();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetActiveUniform')
end;
procedure glCapGetAttachedShaders();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetAttachedShaders')
end;
procedure glCapGetAttribLocation();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetAttribLocation')
end;
procedure glCapGetProgramiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetProgramiv')
end;
procedure glCapGetProgramInfoLog();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetProgramInfoLog')
end;
procedure glCapGetShaderiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetShaderiv')
end;
procedure glCapGetShaderInfoLog();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetShaderInfoLog')
end;
procedure glCapGetShaderSource();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetShaderSource')
end;
procedure glCapGetUniformLocation();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetUniformLocation')
end;
procedure glCapGetUniformfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetUniformfv')
end;
procedure glCapGetUniformiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetUniformiv')
end;
procedure glCapGetVertexAttribdv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetVertexAttribdv')
end;
procedure glCapGetVertexAttribfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetVertexAttribfv')
end;
procedure glCapGetVertexAttribiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetVertexAttribiv')
end;
procedure glCapGetVertexAttribPointerv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetVertexAttribPointerv')
end;
procedure glCapIsProgram();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsProgram')
end;
procedure glCapIsShader();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsShader')
end;
procedure glCapBindAttribLocation();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindAttribLocation')
end;
procedure glCapBindFragDataLocationIndexed();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindFragDataLocationIndexed')
end;
procedure glCapGetFragDataIndex();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetFragDataIndex')
end;
procedure glCapGetVaryingLocation();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetVaryingLocation')
end;
procedure glCapGetActiveVarying();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetActiveVarying')
end;
procedure glCapActiveVarying();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ActiveVarying')
end;
procedure glCapGetProgramBinary();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetProgramBinary')
end;
procedure glCapProgramBinary();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramBinary')
end;
procedure glCapUseProgramStages();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('UseProgramStages')
end;
procedure glCapActiveShaderProgram();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ActiveShaderProgram')
end;
procedure glCapCreateShaderProgramv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CreateShaderProgramv')
end;
procedure glCapBindProgramPipeline();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindProgramPipeline')
end;
procedure glCapDeleteProgramPipelines();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteProgramPipelines')
end;
procedure glCapGenProgramPipelines();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenProgramPipelines')
end;
procedure glCapIsProgramPipeline();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsProgramPipeline')
end;
procedure glCapGetProgramPipelineiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetProgramPipelineiv')
end;
procedure glCapProgramUniform1i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform1i')
end;
procedure glCapProgramUniform1iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform1iv')
end;
procedure glCapProgramUniform1f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform1f')
end;
procedure glCapProgramUniform1fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform1fv')
end;
procedure glCapProgramUniform1d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform1d')
end;
procedure glCapProgramUniform1dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform1dv')
end;
procedure glCapProgramUniform1ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform1ui')
end;
procedure glCapProgramUniform1uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform1uiv')
end;
procedure glCapProgramUniform2i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform2i')
end;
procedure glCapProgramUniform2iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform2iv')
end;
procedure glCapProgramUniform2f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform2f')
end;
procedure glCapProgramUniform2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform2fv')
end;
procedure glCapProgramUniform2d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform2d')
end;
procedure glCapProgramUniform2dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform2dv')
end;
procedure glCapProgramUniform2ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform2ui')
end;
procedure glCapProgramUniform2uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform2uiv')
end;
procedure glCapProgramUniform3i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform3i')
end;
procedure glCapProgramUniform3iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform3iv')
end;
procedure glCapProgramUniform3f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform3f')
end;
procedure glCapProgramUniform3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform3fv')
end;
procedure glCapProgramUniform3d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform3d')
end;
procedure glCapProgramUniform3dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform3dv')
end;
procedure glCapProgramUniform3ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform3ui')
end;
procedure glCapProgramUniform3uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform3uiv')
end;
procedure glCapProgramUniform4i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform4i')
end;
procedure glCapProgramUniform4iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform4iv')
end;
procedure glCapProgramUniform4f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform4f')
end;
procedure glCapProgramUniform4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform4fv')
end;
procedure glCapProgramUniform4d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform4d')
end;
procedure glCapProgramUniform4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform4dv')
end;
procedure glCapProgramUniform4ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform4ui')
end;
procedure glCapProgramUniform4uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniform4uiv')
end;
procedure glCapProgramUniformMatrix2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix2fv')
end;
procedure glCapProgramUniformMatrix3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix3fv')
end;
procedure glCapProgramUniformMatrix4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix4fv')
end;
procedure glCapProgramUniformMatrix2dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix2dv')
end;
procedure glCapProgramUniformMatrix3dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix3dv')
end;
procedure glCapProgramUniformMatrix4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix4dv')
end;
procedure glCapProgramUniformMatrix2x3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix2x3fv')
end;
procedure glCapProgramUniformMatrix3x2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix3x2fv')
end;
procedure glCapProgramUniformMatrix2x4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix2x4fv')
end;
procedure glCapProgramUniformMatrix4x2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix4x2fv')
end;
procedure glCapProgramUniformMatrix3x4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix3x4fv')
end;
procedure glCapProgramUniformMatrix4x3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix4x3fv')
end;
procedure glCapProgramUniformMatrix2x3dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix2x3dv')
end;
procedure glCapProgramUniformMatrix3x2dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix3x2dv')
end;
procedure glCapProgramUniformMatrix2x4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix2x4dv')
end;
procedure glCapProgramUniformMatrix4x2dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix4x2dv')
end;
procedure glCapProgramUniformMatrix3x4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix3x4dv')
end;
procedure glCapProgramUniformMatrix4x3dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramUniformMatrix4x3dv')
end;
procedure glCapValidateProgramPipeline();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ValidateProgramPipeline')
end;
procedure glCapGetProgramPipelineInfoLog();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetProgramPipelineInfoLog')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Framebuffer object'}{$ENDIF}
procedure glCapIsRenderbuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsRenderbuffer')
end;
procedure glCapBindRenderbuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindRenderbuffer')
end;
procedure glCapDeleteRenderbuffers();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteRenderbuffers')
end;
procedure glCapGenRenderbuffers();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenRenderbuffers')
end;
procedure glCapRenderbufferStorage();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RenderbufferStorage')
end;
procedure glCapRenderbufferStorageMultisample();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('RenderbufferStorageMultisample')
end;
procedure glCapGetRenderbufferParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetRenderbufferParameteriv')
end;
procedure glCapIsFramebuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsFramebuffer')
end;
procedure glCapBindFramebuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindFramebuffer')
end;
procedure glCapDeleteFramebuffers();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteFramebuffers')
end;
procedure glCapGenFramebuffers();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenFramebuffers')
end;
procedure glCapCheckFramebufferStatus();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CheckFramebufferStatus')
end;
procedure glCapFramebufferTexture();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FramebufferTexture')
end;
procedure glCapFramebufferTexture1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FramebufferTexture1D')
end;
procedure glCapFramebufferTexture2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FramebufferTexture2D')
end;
procedure glCapFramebufferTexture3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FramebufferTexture3D')
end;
procedure glCapFramebufferTextureLayer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FramebufferTextureLayer')
end;
procedure glCapFramebufferTextureFace();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FramebufferTextureFace')
end;
procedure glCapFramebufferRenderbuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FramebufferRenderbuffer')
end;
procedure glCapGetFramebufferAttachmentParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetFramebufferAttachmentParameteriv')
end;
procedure glCapBlitFramebuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BlitFramebuffer')
end;
procedure glCapGenerateMipmap();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenerateMipmap')
end;
procedure glCapDepthBounds();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DepthBounds')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Queries object'}{$ENDIF}
procedure glCapGenQueries();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenQueries')
end;
procedure glCapDeleteQueries();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteQueries')
end;
procedure glCapIsQuery();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsQuery')
end;
procedure glCapBeginQuery();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BeginQuery')
end;
procedure glCapEndQuery();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EndQuery')
end;
procedure glCapGetQueryiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetQueryiv')
end;
procedure glCapGetQueryObjectiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetQueryObjectiv')
end;
procedure glCapGetQueryObjectuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetQueryObjectuiv')
end;
procedure glCapQueryCounter();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('QueryCounter')
end;
procedure glCapGetQueryObjecti64v();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetQueryObjecti64v')
end;
procedure glCapGetQueryObjectui64v();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetQueryObjectui64v')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Sampler object'}{$ENDIF}
procedure glCapActiveTexture();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ActiveTexture')
end;
procedure glCapSampleCoverage();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('SampleCoverage')
end;
procedure glCapCompressedTexImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTexImage3D')
end;
procedure glCapCompressedTexImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTexImage2D')
end;
procedure glCapCompressedTexImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTexImage1D')
end;
procedure glCapCompressedTexSubImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTexSubImage3D')
end;
procedure glCapCompressedTexSubImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTexSubImage2D')
end;
procedure glCapCompressedTexSubImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTexSubImage1D')
end;
procedure glCapGetCompressedTexImage();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetCompressedTexImage')
end;
procedure glCapClientActiveTexture();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClientActiveTexture')
end;
procedure glCapMultiTexCoord1d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord1d')
end;
procedure glCapMultiTexCoord1dV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord1dV')
end;
procedure glCapMultiTexCoord1f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord1f')
end;
procedure glCapMultiTexCoord1fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord1fv')
end;
procedure glCapMultiTexCoord1i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord1i')
end;
procedure glCapMultiTexCoord1iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord1iv')
end;
procedure glCapMultiTexCoord1s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord1s')
end;
procedure glCapMultiTexCoord1sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord1sv')
end;
procedure glCapMultiTexCoord2d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord2d')
end;
procedure glCapMultiTexCoord2dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord2dv')
end;
procedure glCapMultiTexCoord2f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord2f')
end;
procedure glCapMultiTexCoord2fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord2fv')
end;
procedure glCapMultiTexCoord2i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord2i')
end;
procedure glCapMultiTexCoord2iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord2iv')
end;
procedure glCapMultiTexCoord2s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord2s')
end;
procedure glCapMultiTexCoord2sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord2sv')
end;
procedure glCapMultiTexCoord3d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord3d')
end;
procedure glCapMultiTexCoord3dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord3dv')
end;
procedure glCapMultiTexCoord3f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord3f')
end;
procedure glCapMultiTexCoord3fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord3fv')
end;
procedure glCapMultiTexCoord3i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord3i')
end;
procedure glCapMultiTexCoord3iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord3iv')
end;
procedure glCapMultiTexCoord3s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord3s')
end;
procedure glCapMultiTexCoord3sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord3sv')
end;
procedure glCapMultiTexCoord4d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord4d')
end;
procedure glCapMultiTexCoord4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord4dv')
end;
procedure glCapMultiTexCoord4f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord4f')
end;
procedure glCapMultiTexCoord4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord4fv')
end;
procedure glCapMultiTexCoord4i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord4i')
end;
procedure glCapMultiTexCoord4iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord4iv')
end;
procedure glCapMultiTexCoord4s();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord4s')
end;
procedure glCapMultiTexCoord4sv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoord4sv')
end;
procedure glCapGetInteger64i_v();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetInteger64i_v')
end;
procedure glCapGetBufferParameteri64v();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetBufferParameteri64v')
end;
procedure glCapProgramParameteri();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramParameteri')
end;
procedure glCapProgramString();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramString')
end;
procedure glCapBindProgram();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindProgram')
end;
procedure glCapDeletePrograms();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeletePrograms')
end;
procedure glCapGenPrograms();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenPrograms')
end;
procedure glCapProgramEnvParameter4d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramEnvParameter4d')
end;
procedure glCapProgramEnvParameter4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramEnvParameter4dv')
end;
procedure glCapProgramEnvParameter4f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramEnvParameter4f')
end;
procedure glCapProgramEnvParameter4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramEnvParameter4fv')
end;
procedure glCapProgramLocalParameter4d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramLocalParameter4d')
end;
procedure glCapProgramLocalParameter4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramLocalParameter4dv')
end;
procedure glCapProgramLocalParameter4f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramLocalParameter4f')
end;
procedure glCapProgramLocalParameter4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProgramLocalParameter4fv')
end;
procedure glCapGetProgramEnvParameterdv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetProgramEnvParameterdv')
end;
procedure glCapGetProgramEnvParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetProgramEnvParameterfv')
end;
procedure glCapGetProgramLocalParameterdv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetProgramLocalParameterdv')
end;
procedure glCapGetProgramLocalParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetProgramLocalParameterfv')
end;
procedure glCapTexImage2DMultisample();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexImage2DMultisample')
end;
procedure glCapTexImage3DMultisample();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TexImage3DMultisample')
end;
procedure glCapGetMultisamplefv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultisamplefv')
end;
procedure glCapSampleMaski();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('SampleMaski')
end;
procedure glCapProvokingVertex();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ProvokingVertex')
end;
procedure glCapFenceSync();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FenceSync')
end;
procedure glCapIsSync();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsSync')
end;
procedure glCapDeleteSync();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteSync')
end;
procedure glCapClientWaitSync();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClientWaitSync')
end;
procedure glCapWaitSync();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WaitSync')
end;
procedure glCapGetInteger64v();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetInteger64v')
end;
procedure glCapGetSynciv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetSynciv')
end;
procedure glCapBlendEquationi();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BlendEquationi')
end;
procedure glCapBlendEquationSeparatei();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BlendEquationSeparatei')
end;
procedure glCapBlendFunci();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BlendFunci')
end;
procedure glCapBlendFuncSeparatei();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BlendFuncSeparatei')
end;
procedure glCapMinSampleShading();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MinSampleShading')
end;
procedure glCapGenSamplers();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenSamplers')
end;
procedure glCapDeleteSamplers();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeleteSamplers')
end;
procedure glCapIsSampler();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsSampler')
end;
procedure glCapBindSampler();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindSampler')
end;
procedure glCapSamplerParameteri();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('SamplerParameteri')
end;
procedure glCapSamplerParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('SamplerParameteriv')
end;
procedure glCapSamplerParameterf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('SamplerParameterf')
end;
procedure glCapSamplerParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('SamplerParameterfv')
end;
procedure glCapSamplerParameterIiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('SamplerParameterIiv')
end;
procedure glCapSamplerParameterIuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('SamplerParameterIuiv')
end;
procedure glCapGetSamplerParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetSamplerParameteriv')
end;
procedure glCapGetSamplerParameterIiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetSamplerParameterIiv')
end;
procedure glCapGetSamplerParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetSamplerParameterfv')
end;
procedure glCapGetSamplerParameterIfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetSamplerParameterIfv')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Direct access'}{$ENDIF}
procedure glCapClientAttribDefault();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ClientAttribDefault')
end;
procedure glCapPushClientAttribDefault();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PushClientAttribDefault')
end;
procedure glCapMatrixLoadf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixLoadf')
end;
procedure glCapMatrixLoadd();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixLoadd')
end;
procedure glCapMatrixMultf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixMultf')
end;
procedure glCapMatrixMultd();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixMultd')
end;
procedure glCapMatrixLoadIdentity();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixLoadIdentity')
end;
procedure glCapMatrixRotatef();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixRotatef')
end;
procedure glCapMatrixRotated();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixRotated')
end;
procedure glCapMatrixScalef();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixScalef')
end;
procedure glCapMatrixScaled();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixScaled')
end;
procedure glCapMatrixTranslatef();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixTranslatef')
end;
procedure glCapMatrixTranslated();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixTranslated')
end;
procedure glCapMatrixFrustum();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixFrustum')
end;
procedure glCapMatrixOrtho();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixOrtho')
end;
procedure glCapMatrixPop();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixPop')
end;
procedure glCapMatrixPush();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixPush')
end;
procedure glCapMatrixLoadTransposef();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixLoadTransposef')
end;
procedure glCapMatrixLoadTransposed();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixLoadTransposed')
end;
procedure glCapMatrixMultTransposef();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixMultTransposef')
end;
procedure glCapMatrixMultTransposed();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MatrixMultTransposed')
end;
procedure glCapTextureParameterf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureParameterf')
end;
procedure glCapTextureParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureParameterfv')
end;
procedure glCapTextureParameteri();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureParameteri')
end;
procedure glCapTextureParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureParameteriv')
end;
procedure glCapTextureImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureImage1D')
end;
procedure glCapTextureImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureImage2D')
end;
procedure glCapTextureSubImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureSubImage1D')
end;
procedure glCapTextureSubImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureSubImage2D')
end;
procedure glCapCopyTextureImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyTextureImage1D')
end;
procedure glCapCopyTextureImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyTextureImage2D')
end;
procedure glCapCopyTextureSubImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyTextureSubImage1D')
end;
procedure glCapCopyTextureSubImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyTextureSubImage2D')
end;
procedure glCapGetTextureImage();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTextureImage')
end;
procedure glCapGetTextureParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTextureParameterfv')
end;
procedure glCapGetTextureParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTextureParameteriv')
end;
procedure glCapGetTextureLevelParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTextureLevelParameterfv')
end;
procedure glCapGetTextureLevelParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTextureLevelParameteriv')
end;
procedure glCapTextureImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureImage3D')
end;
procedure glCapTextureSubImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureSubImage3D')
end;
procedure glCapCopyTextureSubImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyTextureSubImage3D')
end;
procedure glCapMultiTexParameterf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexParameterf')
end;
procedure glCapMultiTexParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexParameterfv')
end;
procedure glCapMultiTexParameteri();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexParameteri')
end;
procedure glCapMultiTexParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexParameteriv')
end;
procedure glCapMultiTexImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexImage1D')
end;
procedure glCapMultiTexImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexImage2D')
end;
procedure glCapMultiTexSubImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexSubImage1D')
end;
procedure glCapMultiTexSubImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexSubImage2D')
end;
procedure glCapCopyMultiTexImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyMultiTexImage1D')
end;
procedure glCapCopyMultiTexImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyMultiTexImage2D')
end;
procedure glCapCopyMultiTexSubImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyMultiTexSubImage1D')
end;
procedure glCapCopyMultiTexSubImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyMultiTexSubImage2D')
end;
procedure glCapGetMultiTexImage();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexImage')
end;
procedure glCapGetMultiTexParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexParameterfv')
end;
procedure glCapGetMultiTexParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexParameteriv')
end;
procedure glCapGetMultiTexLevelParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexLevelParameterfv')
end;
procedure glCapGetMultiTexLevelParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexLevelParameteriv')
end;
procedure glCapMultiTexImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexImage3D')
end;
procedure glCapMultiTexSubImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexSubImage3D')
end;
procedure glCapCopyMultiTexSubImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyMultiTexSubImage3D')
end;
procedure glCapBindMultiTexture();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('BindMultiTexture')
end;
procedure glCapEnableClientStateIndexed();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('EnableClientStateIndexed')
end;
procedure glCapDisableClientStateIndexed();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DisableClientStateIndexed')
end;
procedure glCapMultiTexCoordPointer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexCoordPointer')
end;
procedure glCapMultiTexEnvf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexEnvf')
end;
procedure glCapMultiTexEnvfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexEnvfv')
end;
procedure glCapMultiTexEnvi();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexEnvi')
end;
procedure glCapMultiTexEnviv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexEnviv')
end;
procedure glCapMultiTexGend();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexGend')
end;
procedure glCapMultiTexGendv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexGendv')
end;
procedure glCapMultiTexGenf();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexGenf')
end;
procedure glCapMultiTexGenfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexGenfv')
end;
procedure glCapMultiTexGeni();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexGeni')
end;
procedure glCapMultiTexGeniv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexGeniv')
end;
procedure glCapGetMultiTexEnvfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexEnvfv')
end;
procedure glCapGetMultiTexEnviv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexEnviv')
end;
procedure glCapGetMultiTexGendv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexGendv')
end;
procedure glCapGetMultiTexGenfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexGenfv')
end;
procedure glCapGetMultiTexGeniv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexGeniv')
end;
procedure glCapGetFloatIndexedv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetFloatIndexedv')
end;
procedure glCapGetDoubleIndexedv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetDoubleIndexedv')
end;
procedure glCapGetPointerIndexedv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPointerIndexedv')
end;
procedure glCapCompressedTextureImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTextureImage3D')
end;
procedure glCapCompressedTextureImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTextureImage2D')
end;
procedure glCapCompressedTextureImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTextureImage1D')
end;
procedure glCapCompressedTextureSubImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTextureSubImage3D')
end;
procedure glCapCompressedTextureSubImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTextureSubImage2D')
end;
procedure glCapCompressedTextureSubImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedTextureSubImage1D')
end;
procedure glCapGetCompressedTextureImage();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetCompressedTextureImage')
end;
procedure glCapCompressedMultiTexImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedMultiTexImage3D')
end;
procedure glCapCompressedMultiTexImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedMultiTexImage2D')
end;
procedure glCapCompressedMultiTexImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedMultiTexImage1D')
end;
procedure glCapCompressedMultiTexSubImage3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedMultiTexSubImage3D')
end;
procedure glCapCompressedMultiTexSubImage2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedMultiTexSubImage2D')
end;
procedure glCapCompressedMultiTexSubImage1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CompressedMultiTexSubImage1D')
end;
procedure glCapGetCompressedMultiTexImage();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetCompressedMultiTexImage')
end;
procedure glCapNamedProgramString();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramString')
end;
procedure glCapNamedProgramLocalParameter4d();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramLocalParameter4d')
end;
procedure glCapNamedProgramLocalParameter4dv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramLocalParameter4dv')
end;
procedure glCapNamedProgramLocalParameter4f();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramLocalParameter4f')
end;
procedure glCapNamedProgramLocalParameter4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramLocalParameter4fv')
end;
procedure glCapGetNamedProgramLocalParameterdv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedProgramLocalParameterdv')
end;
procedure glCapGetNamedProgramLocalParameterfv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedProgramLocalParameterfv')
end;
procedure glCapGetNamedProgramiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedProgramiv')
end;
procedure glCapGetNamedProgramString();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedProgramString')
end;
procedure glCapNamedProgramLocalParameters4fv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramLocalParameters4fv')
end;
procedure glCapNamedProgramLocalParameterI4i();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramLocalParameterI4i')
end;
procedure glCapNamedProgramLocalParameterI4iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramLocalParameterI4iv')
end;
procedure glCapNamedProgramLocalParametersI4iv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramLocalParametersI4iv')
end;
procedure glCapNamedProgramLocalParameterI4ui();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramLocalParameterI4ui')
end;
procedure glCapNamedProgramLocalParameterI4uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramLocalParameterI4uiv')
end;
procedure glCapNamedProgramLocalParametersI4uiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedProgramLocalParametersI4uiv')
end;
procedure glCapGetNamedProgramLocalParameterIiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedProgramLocalParameterIiv')
end;
procedure glCapGetNamedProgramLocalParameterIuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedProgramLocalParameterIuiv')
end;
procedure glCapTextureParameterIiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureParameterIiv')
end;
procedure glCapTextureParameterIuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureParameterIuiv')
end;
procedure glCapGetTextureParameterIiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTextureParameterIiv')
end;
procedure glCapGetTextureParameterIuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetTextureParameterIuiv')
end;
procedure glCapMultiTexParameterIiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexParameterIiv')
end;
procedure glCapMultiTexParameterIuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexParameterIuiv')
end;
procedure glCapGetMultiTexParameterIiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexParameterIiv')
end;
procedure glCapGetMultiTexParameterIuiv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetMultiTexParameterIuiv')
end;
procedure glCapNamedBufferData();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedBufferData')
end;
procedure glCapNamedBufferSubData();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedBufferSubData')
end;
procedure glCapMapNamedBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MapNamedBuffer')
end;
procedure glCapUnmapNamedBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('UnmapNamedBuffer')
end;
procedure glCapMapNamedBufferRange();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MapNamedBufferRange')
end;
procedure glCapFlushMappedNamedBufferRange();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FlushMappedNamedBufferRange')
end;
procedure glCapNamedCopyBufferSubData();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedCopyBufferSubData')
end;
procedure glCapGetNamedBufferParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedBufferParameteriv')
end;
procedure glCapGetNamedBufferPointerv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedBufferPointerv')
end;
procedure glCapGetNamedBufferSubData();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedBufferSubData')
end;
procedure glCapTextureBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureBuffer')
end;
procedure glCapMultiTexBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexBuffer')
end;
procedure glCapNamedRenderbufferStorage();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedRenderbufferStorage')
end;
procedure glCapGetNamedRenderbufferParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedRenderbufferParameteriv')
end;
procedure glCapCheckNamedFramebufferStatus();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CheckNamedFramebufferStatus')
end;
procedure glCapNamedFramebufferTexture1D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedFramebufferTexture1D')
end;
procedure glCapNamedFramebufferTexture2D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedFramebufferTexture2D')
end;
procedure glCapNamedFramebufferTexture3D();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedFramebufferTexture3D')
end;
procedure glCapNamedFramebufferRenderbuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedFramebufferRenderbuffer')
end;
procedure glCapGetNamedFramebufferAttachmentParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetNamedFramebufferAttachmentParameteriv')
end;
procedure glCapGenerateTextureMipmap();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenerateTextureMipmap')
end;
procedure glCapGenerateMultiTexMipmap();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenerateMultiTexMipmap')
end;
procedure glCapFramebufferDrawBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FramebufferDrawBuffer')
end;
procedure glCapFramebufferDrawBuffers();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FramebufferDrawBuffers')
end;
procedure glCapFramebufferReadBuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FramebufferReadBuffer')
end;
procedure glCapGetFramebufferParameteriv();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetFramebufferParameteriv')
end;
procedure glCapNamedRenderbufferStorageMultisample();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedRenderbufferStorageMultisample')
end;
procedure glCapNamedRenderbufferStorageMultisampleCoverage();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedRenderbufferStorageMultisampleCoverage')
end;
procedure glCapNamedFramebufferTexture();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedFramebufferTexture')
end;
procedure glCapNamedFramebufferTextureLayer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedFramebufferTextureLayer')
end;
procedure glCapNamedFramebufferTextureFace();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('NamedFramebufferTextureFace')
end;
procedure glCapTextureRenderbuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('TextureRenderbuffer')
end;
procedure glCapMultiTexRenderbuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('MultiTexRenderbuffer')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Debugging'}{$ENDIF}
procedure glCapFrameTerminatorGREMEDY();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('FrameTerminatorGREMEDY')
end;
procedure glCapStringMarkerGREMEDY();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('StringMarkerGREMEDY')
end;
procedure glCapDebugMessageEnableAMDX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DebugMessageEnableAMDX')
end;
procedure glCapDebugMessageCallbackAMDX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DebugMessageCallbackAMDX')
end;
procedure glCapDebugMessageControl();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DebugMessageControl')
end;
procedure glCapDebugMessageInsert();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DebugMessageInsert')
end;
procedure glCapDebugMessageCallback();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DebugMessageCallback')
end;
procedure glCapGetDebugMessageLog();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetDebugMessageLog')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Interrop'}{$ENDIF}
{$IFDEF LINUX}
procedure glCapVDPAUInitNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VDPAUInitNV')
end;
procedure glCapVDPAUFiniNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VDPAUFiniNV')
end;
procedure glCapVDPAURegisterVideoSurfaceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VDPAURegisterVideoSurfaceNV')
end;
procedure glCapVDPAURegisterOutputSurfaceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VDPAURegisterOutputSurfaceNV')
end;
procedure glCapVDPAUIsSurfaceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VDPAUIsSurfaceNV')
end;
procedure glCapVDPAUUnregisterSurfaceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VDPAUUnregisterSurfaceNV')
end;
procedure glCapVDPAUGetSurfaceivNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VDPAUGetSurfaceivNV')
end;
procedure glCapVDPAUSurfaceAccessNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VDPAUSurfaceAccessNV')
end;
procedure glCapVDPAUMapSurfacesNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VDPAUMapSurfacesNV')
end;
procedure glCapVDPAUUnmapSurfacesNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('VDPAUUnmapSurfacesNV')
end;
{$ENDIF LINUX}
{$IFDEF GLS_REGIONS}{$ENDREGION 'Interrop'}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Path rendering'}{$ENDIF}
procedure glCapGenPathsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GenPathsNV')
end;
procedure glCapDeletePathsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('DeletePathsNV')
end;
procedure glCapIsPathNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsPathNV')
end;
procedure glCapPathCommandsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathCommandsNV')
end;
procedure glCapPathCoordsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathCoordsNV')
end;
procedure glCapPathSubCommandsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathSubCommandsNV')
end;
procedure glCapPathSubCoordsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathSubCoordsNV')
end;
procedure glCapPathStringNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathStringNV')
end;
procedure glCapPathGlyphsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathGlyphsNV')
end;
procedure glCapPathGlyphRangeNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathGlyphRangeNV')
end;
procedure glCapWeightPathsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WeightPathsNV')
end;
procedure glCapCopyPathNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CopyPathNV')
end;
procedure glCapInterpolatePathsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('InterpolatePathsNV')
end;
procedure glCapPathParameterivNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathParameterivNV')
end;
procedure glCapPathParameteriNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathParameteriNV')
end;
procedure glCapPathParameterfvNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathParameterfvNV')
end;
procedure glCapPathParameterfNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathParameterfNV')
end;
procedure glCapPathDashArrayNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathDashArrayNV')
end;
procedure glCapPathStencilFuncNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathStencilFuncNV')
end;
procedure glCapStencilFillPathNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('StencilFillPathNV')
end;
procedure glCapStencilStrokePathNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('StencilStrokePathNV')
end;
procedure glCapStencilFillPathInstancedNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('StencilFillPathInstancedNV')
end;
procedure glCapStencilStrokePathInstancedNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('StencilStrokePathInstancedNV')
end;
procedure glCapPathColorGenNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathColorGenNV')
end;
procedure glCapPathTexGenNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathTexGenNV')
end;
procedure glCapPathFogGenNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathFogGenNV')
end;
procedure glCapCoverFillPathNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CoverFillPathNV')
end;
procedure glCapCoverStrokePathNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CoverStrokePathNV')
end;
procedure glCapCoverFillPathInstancedNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CoverFillPathInstancedNV')
end;
procedure glCapCoverStrokePathInstancedNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('CoverStrokePathInstancedNV')
end;
procedure glCapGetPathParameterivNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathParameterivNV')
end;
procedure glCapGetPathParameterfvNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathParameterfvNV')
end;
procedure glCapGetPathCommandsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathCommandsNV')
end;
procedure glCapGetPathCoordsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathCoordsNV')
end;
procedure glCapGetPathDashArrayNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathDashArrayNV')
end;
procedure glCapGetPathMetricsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathMetricsNV')
end;
procedure glCapGetPathMetricRangeNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathMetricRangeNV')
end;
procedure glCapGetPathSpacingNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathSpacingNV')
end;
procedure glCapGetPathColorGenivNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathColorGenivNV')
end;
procedure glCapGetPathColorGenfvNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathColorGenfvNV')
end;
procedure glCapGetPathTexGenivNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathTexGenivNV')
end;
procedure glCapGetPathTexGenfvNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathTexGenfvNV')
end;
procedure glCapIsPointInFillPathNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsPointInFillPathNV')
end;
procedure glCapIsPointInStrokePathNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('IsPointInStrokePathNV')
end;
procedure glCapGetPathLengthNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('GetPathLengthNV')
end;
procedure glCapPointAlongPathNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PointAlongPathNV')
end;
procedure glCapPathStencilDepthOffsetNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathStencilDepthOffsetNV')
end;
procedure glCapPathCoverDepthFuncNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('PathCoverDepthFuncNV')
end;
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}
{$REGION 'Windows OpenGL (WGL) function/procedure definitions for ARB approved extensions'}
{$ENDIF}
{$IFDEF SUPPORT_WGL}
procedure glCapWCreateBufferRegionARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WCreateBufferRegionARB')
end;
procedure glCapWDeleteBufferRegionARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WDeleteBufferRegionARB')
end;
procedure glCapWSaveBufferRegionARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WSaveBufferRegionARB')
end;
procedure glCapWRestoreBufferRegionARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WRestoreBufferRegionARB')
end;
procedure glCapWGetExtensionsStringARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WGetExtensionsStringARB')
end;
procedure glCapWGetPixelFormatAttribivARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WGetPixelFormatAttribivARB')
end;
procedure glCapWGetPixelFormatAttribfvARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WGetPixelFormatAttribfvARB')
end;
procedure glCapWChoosePixelFormatARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WChoosePixelFormatARB')
end;
procedure glCapWMakeContextCurrentARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WMakeContextCurrentARB')
end;
procedure glCapWGetCurrentReadDCARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WGetCurrentReadDCARB')
end;
procedure glCapWCreatePbufferARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WCreatePbufferARB')
end;
procedure glCapWGetPbufferDCARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WGetPbufferDCARB')
end;
procedure glCapWReleasePbufferDCARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WReleasePbufferDCARB')
end;
procedure glCapWDestroyPbufferARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WDestroyPbufferARB')
end;
procedure glCapWQueryPbufferARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WQueryPbufferARB')
end;
procedure glCapWBindTexImageARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WBindTexImageARB')
end;
procedure glCapWReleaseTexImageARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WReleaseTexImageARB')
end;
procedure glCapWSetPbufferAttribARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WSetPbufferAttribARB')
end;
procedure glCapWCreateContextAttribsARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WCreateContextAttribsARB')
end;
procedure glCapWEnumGpusNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WEnumGpusNV')
end;
procedure glCapWEnumGpuDevicesNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WEnumGpuDevicesNV')
end;
procedure glCapWCreateAffinityDCNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WCreateAffinityDCNV')
end;
procedure glCapWEnumGpusFromAffinityDCNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WEnumGpusFromAffinityDCNV')
end;
procedure glCapWDeleteDCNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WDeleteDCNV')
end;
procedure glCapWDXSetResourceShareHandleNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WDXSetResourceShareHandleNV')
end;
procedure glCapWDXOpenDeviceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WDXOpenDeviceNV')
end;
procedure glCapWDXCloseDeviceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WDXCloseDeviceNV')
end;
procedure glCapWDXRegisterObjectNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WDXRegisterObjectNV')
end;
procedure glCapWDXUnregisterObjectNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WDXUnregisterObjectNV')
end;
procedure glCapWDXObjectAccessNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WDXObjectAccessNV')
end;
procedure glCapWDXLockObjectsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WDXLockObjectsNV')
end;
procedure glCapWDXUnlockObjectsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WDXUnlockObjectsNV')
end;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}
{$REGION 'Windows OpenGL (WGL) function/procedure definitions for Vendor/EXT extensions'}
{$ENDIF}
{$IFDEF SUPPORT_WGL}
procedure glCapWSwapIntervalEXT();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WSwapIntervalEXT')
end;
procedure glCapWGetSwapIntervalEXT();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WGetSwapIntervalEXT')
end;
procedure glCapWAllocateMemoryNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WAllocateMemoryNV')
end;
procedure glCapWFreeMemoryNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('WFreeMemoryNV')
end;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'GLX function/procedure definitions for ARB approved extensions'}
{$ENDIF}
{$IFDEF SUPPORT_GLX}
procedure glCapXChooseFBConfig();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XChooseFBConfig')
end;
procedure glCapXGetFBConfigAttrib();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetFBConfigAttrib')
end;
procedure glCapXGetFBConfigs();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetFBConfigs')
end;
procedure glCapXGetVisualFromFBConfig();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetVisualFromFBConfig')
end;
procedure glCapXCreateWindow();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCreateWindow')
end;
procedure glCapXDestroyWindow();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XDestroyWindow')
end;
procedure glCapXCreatePixmap();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCreatePixmap')
end;
procedure glCapXDestroyPixmap();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XDestroyPixmap')
end;
procedure glCapXCreatePbuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCreatePbuffer')
end;
procedure glCapXDestroyPbuffer();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XDestroyPbuffer')
end;
procedure glCapXQueryDrawable();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryDrawable')
end;
procedure glCapXCreateNewContext();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCreateNewContext')
end;
procedure glCapXMakeContextCurrent();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XMakeContextCurrent')
end;
procedure glCapXGetCurrentReadDrawable();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetCurrentReadDrawable')
end;
procedure glCapXQueryContext();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryContext')
end;
procedure glCapXSelectEvent();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XSelectEvent')
end;
procedure glCapXGetSelectedEvent();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetSelectedEvent')
end;
procedure glCapXBindTexImageARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XBindTexImageARB')
end;
procedure glCapXReleaseTexImageARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XReleaseTexImageARB')
end;
procedure glCapXDrawableAttribARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XDrawableAttribARB')
end;
procedure glCapXCreateContextAttribsARB();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCreateContextAttribsARB')
end;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'GLX function/procedure definitions for Vendor/EXT extensions'}
{$ENDIF}
{$IFDEF SUPPORT_GLX}
procedure glCapXSwapIntervalSGI();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XSwapIntervalSGI')
end;
procedure glCapXGetVideoSyncSGI();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetVideoSyncSGI')
end;
procedure glCapXWaitVideoSyncSGI();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XWaitVideoSyncSGI')
end;
procedure glCapXFreeContextEXT();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XFreeContextEXT')
end;
procedure glCapXGetContextIDEXT();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetContextIDEXT')
end;
procedure glCapXGetCurrentDisplayEXT();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetCurrentDisplayEXT')
end;
procedure glCapXImportContextEXT();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XImportContextEXT')
end;
procedure glCapXQueryContextInfoEXT();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryContextInfoEXT')
end;
procedure glCapXCopySubBufferMESA();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCopySubBufferMESA')
end;
procedure glCapXCreateGLXPixmapMESA();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCreateGLXPixmapMESA')
end;
procedure glCapXReleaseBuffersMESA();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XReleaseBuffersMESA')
end;
procedure glCapXSet3DfxModeMESA();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XSet3DfxModeMESA')
end;
procedure glCapXBindTexImageEXT();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XBindTexImageEXT')
end;
procedure glCapXReleaseTexImageEXT();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XReleaseTexImageEXT')
end;
procedure glCapXMakeCurrentReadSGI();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XMakeCurrentReadSGI')
end;
procedure glCapXGetCurrentReadDrawableSGI();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetCurrentReadDrawableSGI')
end;
procedure glCapXGetFBConfigAttribSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetFBConfigAttribSGIX')
end;
procedure glCapXChooseFBConfigSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XChooseFBConfigSGIX')
end;
procedure glCapXCreateGLXPixmapWithConfigSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCreateGLXPixmapWithConfigSGIX')
end;
procedure glCapXCreateContextWithConfigSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCreateContextWithConfigSGIX')
end;
procedure glCapXGetVisualFromFBConfigSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetVisualFromFBConfigSGIX')
end;
procedure glCapXGetFBConfigFromVisualSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetFBConfigFromVisualSGIX')
end;
procedure glCapXCreateGLXPbufferSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCreateGLXPbufferSGIX')
end;
procedure glCapXDestroyGLXPbufferSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XDestroyGLXPbufferSGIX')
end;
procedure glCapXQueryGLXPbufferSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryGLXPbufferSGIX')
end;
procedure glCapXSelectEventSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XSelectEventSGIX')
end;
procedure glCapXGetSelectedEventSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetSelectedEventSGIX')
end;
procedure glCapXCushionSGI();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCushionSGI')
end;
procedure glCapXBindChannelToWindowSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XBindChannelToWindowSGIX')
end;
procedure glCapXChannelRectSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XChannelRectSGIX')
end;
procedure glCapXQueryChannelRectSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryChannelRectSGIX')
end;
procedure glCapXQueryChannelDeltasSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryChannelDeltasSGIX')
end;
procedure glCapXChannelRectSyncSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XChannelRectSyncSGIX')
end;
procedure glCapXJoinSwapGroupSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XJoinSwapGroupSGIX')
end;
procedure glCapXBindSwapBarrierSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XBindSwapBarrierSGIX')
end;
procedure glCapXQueryMaxSwapBarriersSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryMaxSwapBarriersSGIX')
end;
procedure glCapXQueryHyperpipeNetworkSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryHyperpipeNetworkSGIX')
end;
procedure glCapXHyperpipeConfigSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XHyperpipeConfigSGIX')
end;
procedure glCapXQueryHyperpipeConfigSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryHyperpipeConfigSGIX')
end;
procedure glCapXDestroyHyperpipeConfigSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XDestroyHyperpipeConfigSGIX')
end;
procedure glCapXBindHyperpipeSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XBindHyperpipeSGIX')
end;
procedure glCapXQueryHyperpipeBestAttribSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryHyperpipeBestAttribSGIX')
end;
procedure glCapXHyperpipeAttribSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XHyperpipeAttribSGIX')
end;
procedure glCapXQueryHyperpipeAttribSGIX();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryHyperpipeAttribSGIX')
end;
procedure glCapXGetAGPOffsetMESA();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetAGPOffsetMESA')
end;
procedure glCapXEnumerateVideoDevicesNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XEnumerateVideoDevicesNV')
end;
procedure glCapXBindVideoDeviceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XBindVideoDeviceNV')
end;
procedure glCapXGetVideoDeviceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetVideoDeviceNV')
end;
procedure glCapXAllocateMemoryNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XAllocateMemoryNV')
end;
procedure glCapXFreeMemoryNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XFreeMemoryNV')
end;
procedure glCapXReleaseVideoDeviceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XReleaseVideoDeviceNV')
end;
procedure glCapXBindVideoImageNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XBindVideoImageNV')
end;
procedure glCapXReleaseVideoImageNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XReleaseVideoImageNV')
end;
procedure glCapXSendPbufferToVideoNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XSendPbufferToVideoNV')
end;
procedure glCapXGetVideoInfoNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XGetVideoInfoNV')
end;
procedure glCapXJoinSwapGroupNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XJoinSwapGroupNV')
end;
procedure glCapXBindSwapBarrierNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XBindSwapBarrierNV')
end;
procedure glCapXQuerySwapGroupNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQuerySwapGroupNV')
end;
procedure glCapXQueryMaxSwapGroupsNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryMaxSwapGroupsNV')
end;
procedure glCapXQueryFrameCountNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryFrameCountNV')
end;
procedure glCapXResetFrameCountNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XResetFrameCountNV')
end;
procedure glCapXBindVideoCaptureDeviceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XBindVideoCaptureDeviceNV')
end;
procedure glCapXEnumerateVideoCaptureDevicesNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XEnumerateVideoCaptureDevicesNV')
end;
procedure glCapXLockVideoCaptureDeviceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XLockVideoCaptureDeviceNV')
end;
procedure glCapXQueryVideoCaptureDeviceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XQueryVideoCaptureDeviceNV')
end;
procedure glCapXReleaseVideoCaptureDeviceNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XReleaseVideoCaptureDeviceNV')
end;
procedure glCapXSwapIntervalEXT();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XSwapIntervalEXT')
end;
procedure glCapXCopyImageSubDataNV();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('XCopyImageSubDataNV')
end;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'AGL function/procedure'}{$ENDIF}
{$IFDEF DARWIN}
procedure glCapACreatePixelFormat();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ACreatePixelFormat')
end;
procedure glCapAChoosePixelFormat ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AChoosePixelFormat ')
end;
procedure glCapADestroyPixelFormat();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ADestroyPixelFormat')
end;
procedure glCapADescribePixelFormat ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ADescribePixelFormat ')
end;
procedure glCapAGetCGLPixelFormat ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AGetCGLPixelFormat ')
end;
procedure glCapADisplaysOfPixelFormat ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ADisplaysOfPixelFormat ')
end;
procedure glCapANextPixelFormat ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ANextPixelFormat ')
end;
procedure glCapACreateContext();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ACreateContext')
end;
procedure glCapACopyContext ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ACopyContext ')
end;
procedure glCapADestroyContext();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ADestroyContext')
end;
procedure glCapAUpdateContext();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AUpdateContext')
end;
procedure glCapASetCurrentContext ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ASetCurrentContext ')
end;
procedure glCapAGetCGLContext ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AGetCGLContext ')
end;
procedure glCapAGetCurrentContext ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AGetCurrentContext ')
end;
procedure glCapASwapBuffers ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ASwapBuffers ')
end;
procedure glCapACreatePBuffer ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ACreatePBuffer ')
end;
procedure glCapADestroyPBuffer ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ADestroyPBuffer ')
end;
procedure glCapADescribePBuffer ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ADescribePBuffer ')
end;
procedure glCapAGetPBuffer ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AGetPBuffer ')
end;
procedure glCapASetPBuffer ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ASetPBuffer ')
end;
procedure glCapATexImagePBuffer ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ATexImagePBuffer ')
end;
procedure glCapASetDrawable();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ASetDrawable')
end;
procedure glCapAGetDrawable();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AGetDrawable')
end;
procedure glCapASetFullScreen();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ASetFullScreen')
end;
procedure glCapASetOffScreen ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ASetOffScreen ')
end;
procedure glCapAEnable ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AEnable ')
end;
procedure glCapADisable ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ADisable ')
end;
procedure glCapAIsEnabled ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AIsEnabled ')
end;
procedure glCapASetInteger ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ASetInteger ')
end;
procedure glCapAGetInteger ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AGetInteger ')
end;
procedure glCapAConfigure ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AConfigure ')
end;
procedure glCapAGetVersion ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AGetVersion ')
end;
procedure glCapAResetLibrary ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AResetLibrary ')
end;
procedure glCapADescribeRenderer ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ADescribeRenderer ')
end;
procedure glCapADestroyRendererInfo ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ADestroyRendererInfo ')
end;
procedure glCapANextRendererInfo ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ANextRendererInfo ')
end;
procedure glCapAQueryRendererInfoForCGDirectDisplayIDs ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AQueryRendererInfoForCGDirectDisplayIDs ')
end;
procedure glCapAGetVirtualScreen ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AGetVirtualScreen ')
end;
procedure glCapASetVirtualScreen ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ASetVirtualScreen ')
end;
procedure glCapASetWindowRef ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ASetWindowRef ')
end;
procedure glCapAGetWindowRef ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AGetWindowRef ')
end;
procedure glCapASetHIViewRef ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('ASetHIViewRef ')
end;
procedure glCapAGetHIViewRef ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AGetHIViewRef ')
end;
procedure glCapAGetError ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AGetError ')
end;
procedure glCapAErrorString ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('AErrorString ')
end;
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'locate functions/procedures for OpenGL Utility (GLU) extensions'} {$ENDIF}
{$IFDEF GLS_REGIONS} {$ENDREGION} {$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'Windows OpenGL (WGL) support functions'}{$ENDIF}
{$IFDEF SUPPORT_WGL}
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'OpenGL Extension to the X Window System (GLX) support functions'}
{$ENDIF}
{$IFDEF SUPPORT_GLX}
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'OpenGL Extension to the Apple System (AGL) support functions'}
{$ENDIF}
{$IFDEF DARWIN}
{$ENDIF}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS}{$REGION 'EGL function/procedure'}{$ENDIF}
{$IFDEF GLS_OPENGL_ES}
procedure glCapeglGetError ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglGetError ')
end;
procedure glCapeglGetDisplay ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglGetDisplay ')
end;
procedure glCapeglInitialize ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglInitialize ')
end;
procedure glCapeglTerminate ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglTerminate ')
end;
procedure glCapeglQueryString ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglQueryString ')
end;
procedure glCapeglGetConfigs ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglGetConfigs ')
end;
procedure glCapeglChooseConfig ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglChooseConfig ')
end;
procedure glCapeglGetConfigAttrib ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglGetConfigAttrib ')
end;
procedure glCapeglCreateWindowSurface ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglCreateWindowSurface ')
end;
procedure glCapeglCreatePbufferSurface ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglCreatePbufferSurface ')
end;
procedure glCapeglCreatePixmapSurface ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglCreatePixmapSurface ')
end;
procedure glCapeglDestroySurface ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglDestroySurface ')
end;
procedure glCapeglQuerySurface ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglQuerySurface ')
end;
procedure glCapeglBindAPI ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglBindAPI ')
end;
procedure glCapeglQueryAPI ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglQueryAPI ')
end;
procedure glCapeglWaitClient ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglWaitClient ')
end;
procedure glCapeglReleaseThread ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglReleaseThread ')
end;
procedure glCapeglCreatePbufferFromClientBuffer ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglCreatePbufferFromClientBuffer ')
end;
procedure glCapeglSurfaceAttrib ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglSurfaceAttrib ')
end;
procedure glCapeglBindTexImage ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglBindTexImage ')
end;
procedure glCapeglReleaseTexImage ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglReleaseTexImage ')
end;
procedure glCapeglSwapInterval ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglSwapInterval ')
end;
procedure glCapeglCreateContext ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglCreateContext ')
end;
procedure glCapeglDestroyContext ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglDestroyContext ')
end;
procedure glCapeglMakeCurrent ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglMakeCurrent ')
end;
procedure glCapeglGetCurrentContext ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglGetCurrentContext ')
end;
procedure glCapeglGetCurrentSurface ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglGetCurrentSurface ')
end;
procedure glCapeglGetCurrentDisplay ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglGetCurrentDisplay ')
end;
procedure glCapeglQueryContext ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglQueryContext ')
end;
procedure glCapeglWaitGL ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglWaitGL ')
end;
procedure glCapeglWaitNative ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglWaitNative ')
end;
procedure glCapeglSwapBuffers ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglSwapBuffers ')
end;
procedure glCapeglCopyBuffers ();
{$IFDEF MSWINDOWS} stdcall;{$ENDIF}
{$IFDEF UNIX} cdecl;{$ENDIF}
begin
   glCap('eglCopyBuffers ')
end;
{$ENDIF GLS_OPENGL_ES}
{$IFDEF GLS_REGIONS}{$ENDREGION}{$ENDIF}
{$IFDEF GLS_REGIONS} {$REGION 'OpenGL utility (GLU) functions and procedures'}
 {$ENDIF}
{$IFDEF GLS_REGIONS} {$ENDREGION} {$ENDIF} 
