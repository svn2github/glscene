// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLAnimatedSprite.pas' rev: 24.00 (Win32)

#ifndef GlanimatedspriteHPP
#define GlanimatedspriteHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <VectorGeometry.hpp>	// Pascal unit
#include <OpenGLTokens.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <XCollection.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLState.hpp>	// Pascal unit
#include <GLCoordinates.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glanimatedsprite
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSpriteAnimFrame;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpriteAnimFrame : public Xcollection::TXCollectionItem
{
	typedef Xcollection::TXCollectionItem inherited;
	
private:
	int FOffsetX;
	int FOffsetY;
	int FWidth;
	int FHeight;
	void __fastcall DoChanged(void);
	
protected:
	void __fastcall SetOffsetX(const int Value);
	void __fastcall SetOffsetY(const int Value);
	void __fastcall SetWidth(const int Value);
	void __fastcall SetHeight(const int Value);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	
public:
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	
__published:
	__property int OffsetX = {read=FOffsetX, write=SetOffsetX, nodefault};
	__property int OffsetY = {read=FOffsetY, write=SetOffsetY, nodefault};
	__property int Width = {read=FWidth, write=SetWidth, nodefault};
	__property int Height = {read=FHeight, write=SetHeight, nodefault};
public:
	/* TXCollectionItem.Create */ inline __fastcall virtual TSpriteAnimFrame(Xcollection::TXCollection* aOwner) : Xcollection::TXCollectionItem(aOwner) { }
	/* TXCollectionItem.Destroy */ inline __fastcall virtual ~TSpriteAnimFrame(void) { }
	
};

#pragma pack(pop)

class DELPHICLASS TSpriteAnimFrameList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpriteAnimFrameList : public Xcollection::TXCollection
{
	typedef Xcollection::TXCollection inherited;
	
public:
	__fastcall virtual TSpriteAnimFrameList(System::Classes::TPersistent* aOwner);
	__classmethod virtual Xcollection::TXCollectionItemClass __fastcall ItemsClass();
public:
	/* TXCollection.Destroy */ inline __fastcall virtual ~TSpriteAnimFrameList(void) { }
	
};

#pragma pack(pop)

enum TSpriteFrameDimensions : unsigned char { sfdAuto, sfdManual };

class DELPHICLASS TSpriteAnimMargins;
class DELPHICLASS TSpriteAnimation;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpriteAnimMargins : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TSpriteAnimation* FOwner;
	int FLeft;
	int FTop;
	int FRight;
	int FBottom;
	
protected:
	void __fastcall SetLeft(const int Value);
	void __fastcall SetTop(const int Value);
	void __fastcall SetRight(const int Value);
	void __fastcall SetBottom(const int Value);
	void __fastcall DoChanged(void);
	
public:
	__fastcall TSpriteAnimMargins(TSpriteAnimation* Animation);
	__property TSpriteAnimation* Owner = {read=FOwner};
	
__published:
	__property int Left = {read=FLeft, write=SetLeft, nodefault};
	__property int Top = {read=FTop, write=SetTop, nodefault};
	__property int Right = {read=FRight, write=SetRight, nodefault};
	__property int Bottom = {read=FBottom, write=SetBottom, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TSpriteAnimMargins(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpriteAnimation : public Xcollection::TXCollectionItem
{
	typedef Xcollection::TXCollectionItem inherited;
	
private:
	int FCurrentFrame;
	int FStartFrame;
	int FEndFrame;
	int FFrameWidth;
	int FFrameHeight;
	int FInterval;
	TSpriteAnimFrameList* FFrames;
	System::UnicodeString FLibMaterialName;
	Glmaterial::TGLLibMaterial* FLibMaterialCached;
	TSpriteFrameDimensions FDimensions;
	TSpriteAnimMargins* FMargins;
	void __fastcall DoChanged(void);
	
protected:
	void __fastcall SetCurrentFrame(const int Value);
	void __fastcall SetFrameWidth(const int Value);
	void __fastcall SetFrameHeight(const int Value);
	virtual void __fastcall WriteToFiler(System::Classes::TWriter* writer);
	virtual void __fastcall ReadFromFiler(System::Classes::TReader* reader);
	void __fastcall SetDimensions(const TSpriteFrameDimensions Value);
	void __fastcall SetLibMaterialName(const System::UnicodeString val);
	Glmaterial::TGLLibMaterial* __fastcall GetLibMaterialCached(void);
	void __fastcall SetInterval(const int Value);
	void __fastcall SetFrameRate(const float Value);
	float __fastcall GetFrameRate(void);
	virtual Glmaterial::TGLAbstractMaterialLibrary* __fastcall GetMaterialLibrary(void);
	
public:
	__fastcall virtual TSpriteAnimation(Xcollection::TXCollection* aOwner);
	__fastcall virtual ~TSpriteAnimation(void);
	__classmethod virtual System::UnicodeString __fastcall FriendlyName();
	__classmethod virtual System::UnicodeString __fastcall FriendlyDescription();
	__property Glmaterial::TGLLibMaterial* LibMaterialCached = {read=GetLibMaterialCached};
	
__published:
	__property int CurrentFrame = {read=FCurrentFrame, write=SetCurrentFrame, nodefault};
	__property int StartFrame = {read=FStartFrame, write=FStartFrame, nodefault};
	__property int EndFrame = {read=FEndFrame, write=FEndFrame, nodefault};
	__property int FrameWidth = {read=FFrameWidth, write=SetFrameWidth, nodefault};
	__property int FrameHeight = {read=FFrameHeight, write=SetFrameHeight, nodefault};
	__property System::UnicodeString LibMaterialName = {read=FLibMaterialName, write=SetLibMaterialName};
	__property TSpriteAnimFrameList* Frames = {read=FFrames};
	__property TSpriteFrameDimensions Dimensions = {read=FDimensions, write=SetDimensions, nodefault};
	__property int Interval = {read=FInterval, write=SetInterval, nodefault};
	__property float FrameRate = {read=GetFrameRate, write=SetFrameRate};
	__property TSpriteAnimMargins* Margins = {read=FMargins};
private:
	void *__IGLMaterialLibrarySupported;	/* Glmaterial::IGLMaterialLibrarySupported */
	
public:
	#if defined(MANAGED_INTERFACE_OPERATORS)
	// {8E442AF9-D212-4A5E-8A88-92F798BABFD1}
	operator Glmaterial::_di_IGLMaterialLibrarySupported()
	{
		Glmaterial::_di_IGLMaterialLibrarySupported intf;
		GetInterface(intf);
		return intf;
	}
	#else
	operator Glmaterial::IGLMaterialLibrarySupported*(void) { return (Glmaterial::IGLMaterialLibrarySupported*)&__IGLMaterialLibrarySupported; }
	#endif
	
};

#pragma pack(pop)

class DELPHICLASS TSpriteAnimationList;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TSpriteAnimationList : public Xcollection::TXCollection
{
	typedef Xcollection::TXCollection inherited;
	
public:
	__fastcall virtual TSpriteAnimationList(System::Classes::TPersistent* aOwner);
	__classmethod virtual Xcollection::TXCollectionItemClass __fastcall ItemsClass();
public:
	/* TXCollection.Destroy */ inline __fastcall virtual ~TSpriteAnimationList(void) { }
	
};

#pragma pack(pop)

enum TSpriteAnimationMode : unsigned char { samNone, samPlayOnce, samLoop, samBounceForward, samBounceBackward, samLoopBackward };

class DELPHICLASS TGLAnimatedSprite;
class PASCALIMPLEMENTATION TGLAnimatedSprite : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	TSpriteAnimationList* FAnimations;
	Glmaterial::TGLMaterialLibrary* FMaterialLibrary;
	int FAnimationIndex;
	int FInterval;
	int FRotation;
	int FPixelRatio;
	bool FMirrorU;
	bool FMirrorV;
	TSpriteAnimationMode FAnimationMode;
	double FCurrentFrameDelta;
	System::Classes::TNotifyEvent FOnFrameChanged;
	System::Classes::TNotifyEvent FOnEndFrameReached;
	System::Classes::TNotifyEvent FOnStartFrameReached;
	
protected:
	virtual void __fastcall DefineProperties(System::Classes::TFiler* Filer);
	void __fastcall WriteAnimations(System::Classes::TStream* Stream);
	void __fastcall ReadAnimations(System::Classes::TStream* Stream);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	void __fastcall SetInterval(const int val);
	void __fastcall SetAnimationIndex(const int val);
	void __fastcall SetAnimationMode(const TSpriteAnimationMode val);
	void __fastcall SetMaterialLibrary(Glmaterial::TGLMaterialLibrary* const val);
	void __fastcall SetPixelRatio(const int val);
	HIDESBASE void __fastcall SetRotation(const int val);
	void __fastcall SetMirrorU(const bool val);
	void __fastcall SetMirrorV(const bool val);
	void __fastcall SetFrameRate(const float Value);
	float __fastcall GetFrameRate(void);
	
public:
	__fastcall virtual TGLAnimatedSprite(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLAnimatedSprite(void);
	virtual void __fastcall BuildList(Glrendercontextinfo::TRenderContextInfo &rci);
	virtual void __fastcall DoProgress(const Baseclasses::TProgressTimes &progressTime);
	void __fastcall NextFrame(void);
	
__published:
	__property TSpriteAnimationList* Animations = {read=FAnimations};
	__property Glmaterial::TGLMaterialLibrary* MaterialLibrary = {read=FMaterialLibrary, write=SetMaterialLibrary};
	__property int Interval = {read=FInterval, write=SetInterval, nodefault};
	__property int AnimationIndex = {read=FAnimationIndex, write=SetAnimationIndex, nodefault};
	__property TSpriteAnimationMode AnimationMode = {read=FAnimationMode, write=SetAnimationMode, nodefault};
	__property int PixelRatio = {read=FPixelRatio, write=SetPixelRatio, nodefault};
	__property int Rotation = {read=FRotation, write=SetRotation, nodefault};
	__property bool MirrorU = {read=FMirrorU, write=SetMirrorU, nodefault};
	__property bool MirrorV = {read=FMirrorV, write=SetMirrorV, nodefault};
	__property float FrameRate = {read=GetFrameRate, write=SetFrameRate};
	__property Position;
	__property Scale;
	__property Visible = {default=1};
	__property System::Classes::TNotifyEvent OnFrameChanged = {read=FOnFrameChanged, write=FOnFrameChanged};
	__property System::Classes::TNotifyEvent OnEndFrameReached = {read=FOnEndFrameReached, write=FOnEndFrameReached};
	__property System::Classes::TNotifyEvent OnStartFrameReached = {read=FOnStartFrameReached, write=FOnStartFrameReached};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLAnimatedSprite(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Glanimatedsprite */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLANIMATEDSPRITE)
using namespace Glanimatedsprite;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlanimatedspriteHPP
