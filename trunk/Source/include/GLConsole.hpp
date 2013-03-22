// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLConsole.pas' rev: 24.00 (Win32)

#ifndef GlconsoleHPP
#define GlconsoleHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.TypInfo.hpp>	// Pascal unit
#include <Vcl.Graphics.hpp>	// Pascal unit
#include <GLScene.hpp>	// Pascal unit
#include <GLObjects.hpp>	// Pascal unit
#include <GLHUDObjects.hpp>	// Pascal unit
#include <GLViewer.hpp>	// Pascal unit
#include <GLBitmapFont.hpp>	// Pascal unit
#include <PersistentClasses.hpp>	// Pascal unit
#include <GLContext.hpp>	// Pascal unit
#include <GLTexture.hpp>	// Pascal unit
#include <GLUtils.hpp>	// Pascal unit
#include <GLStrings.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit
#include <GLMaterial.hpp>	// Pascal unit
#include <GLWin32Viewer.hpp>	// Pascal unit
#include <System.UITypes.hpp>	// Pascal unit
#include <GLRenderContextInfo.hpp>	// Pascal unit
#include <BaseClasses.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glconsole
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EGLConsoleException;
#pragma pack(push,4)
class PASCALIMPLEMENTATION EGLConsoleException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall EGLConsoleException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall EGLConsoleException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size) : System::Sysutils::Exception(Msg, Args, Args_Size) { }
	/* Exception.CreateRes */ inline __fastcall EGLConsoleException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall EGLConsoleException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLConsoleException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size) { }
	/* Exception.CreateResFmt */ inline __fastcall EGLConsoleException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size) { }
	/* Exception.CreateHelp */ inline __fastcall EGLConsoleException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall EGLConsoleException(const System::UnicodeString Msg, System::TVarRec const *Args, const int Args_Size, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLConsoleException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall EGLConsoleException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLConsoleException(System::PResStringRec ResStringRec, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_Size, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall EGLConsoleException(NativeUInt Ident, System::TVarRec const *Args, const int Args_Size, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_Size, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~EGLConsoleException(void) { }
	
};

#pragma pack(pop)

enum TGLConsoleOption : unsigned char { coAutoCompleteCommandsOnKeyPress, coAutoCompleteCommandsOnEnter, coShowConsoleHelpIfUnknownCommand, coRemoveQuotes };

typedef System::Set<TGLConsoleOption, TGLConsoleOption::coAutoCompleteCommandsOnKeyPress, TGLConsoleOption::coRemoveQuotes>  TGLConsoleOptions;

struct DECLSPEC_DRECORD TGLUserInputCommand
{
private:
	typedef System::DynamicArray<System::UnicodeString> _TGLUserInputCommand__1;
	
	
public:
	int CommandCount;
	_TGLUserInputCommand__1 Strings;
	bool UnknownCommand;
};


class DELPHICLASS TGLConsoleCommand;
class DELPHICLASS TGLCustomConsole;
typedef void __fastcall (__closure *TGLlConsoleEvent)(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);

typedef System::Set<System::Int8, 0, 120>  TGLConsoleMatchList;

class DELPHICLASS TGLConsoleStringList;
class PASCALIMPLEMENTATION TGLConsoleStringList : public System::Classes::TStringList
{
	typedef System::Classes::TStringList inherited;
	
private:
	TGLCustomConsole* FConsole;
	
protected:
	virtual void __fastcall Changed(void);
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	bool __fastcall CommandExists(const System::UnicodeString Command);
	__fastcall TGLConsoleStringList(TGLCustomConsole* const Owner);
public:
	/* TStringList.Destroy */ inline __fastcall virtual ~TGLConsoleStringList(void) { }
	
};


class DELPHICLASS TGLConsoleCommandList;
class PASCALIMPLEMENTATION TGLConsoleCommand : public System::Classes::TCollectionItem
{
	typedef System::Classes::TCollectionItem inherited;
	
private:
	bool FVisible;
	bool FEnabled;
	bool FSilentDisabled;
	TGLConsoleCommandList* FCommandList;
	System::UnicodeString FCommandName;
	System::UnicodeString FShortHelp;
	System::Classes::TStringList* FLongHelp;
	TGLlConsoleEvent FOnCommand;
	System::Classes::TNotifyEvent FOnHelp;
	void __fastcall SetCommandName(const System::UnicodeString Value);
	
protected:
	virtual void __fastcall ShowInvalidUseOfCommandError(void);
	virtual void __fastcall ShowInvalidNumberOfArgumentsError(const bool ShowHelpAfter = true);
	virtual void __fastcall DoOnCommand(TGLUserInputCommand &UserInputCommand);
	virtual System::UnicodeString __fastcall GetDisplayName(void);
	
public:
	virtual void __fastcall ShowHelp(void);
	virtual void __fastcall ShowShortHelp(void);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	__fastcall virtual TGLConsoleCommand(System::Classes::TCollection* Collection);
	__fastcall virtual ~TGLConsoleCommand(void);
	
__published:
	__property System::UnicodeString CommandName = {read=FCommandName, write=SetCommandName};
	__property System::UnicodeString ShortHelp = {read=FShortHelp, write=FShortHelp};
	__property System::Classes::TStringList* LongHelp = {read=FLongHelp};
	__property TGLlConsoleEvent OnCommand = {read=FOnCommand, write=FOnCommand};
	__property System::Classes::TNotifyEvent OnHelp = {read=FOnHelp, write=FOnHelp};
	__property bool Enabled = {read=FEnabled, write=FEnabled, default=1};
	__property bool SilentDisabled = {read=FSilentDisabled, write=FSilentDisabled, default=0};
	__property bool Visible = {read=FVisible, write=FVisible, default=1};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLConsoleCommandList : public System::Classes::TCollection
{
	typedef System::Classes::TCollection inherited;
	
public:
	TGLConsoleCommand* operator[](const int Index) { return Items[Index]; }
	
private:
	TGLCustomConsole* FConsole;
	TGLConsoleCommand* __fastcall GetItems(const int Index);
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	void __fastcall SortCommands(const bool Ascending = true);
	bool __fastcall CommandExists(const System::UnicodeString Command);
	int __fastcall GetCommandIndex(const System::UnicodeString Command);
	TGLConsoleCommand* __fastcall LastConsoleCommand(void);
	HIDESBASE TGLConsoleCommand* __fastcall Add(void)/* overload */;
	__fastcall TGLConsoleCommandList(TGLCustomConsole* const AOwner);
	__fastcall virtual ~TGLConsoleCommandList(void);
	__property TGLConsoleCommand* Items[const int Index] = {read=GetItems/*, default*/};
};

#pragma pack(pop)

class DELPHICLASS TGLConsoleControls;
#pragma pack(push,4)
class PASCALIMPLEMENTATION TGLConsoleControls : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TPersistent* FOwner;
	System::Byte FNavigatePageUp;
	System::Byte FAutoCompleteCommand;
	System::Byte FPreviousCommand;
	System::Byte FNextCommand;
	System::Byte FNavigateUp;
	System::Byte FNavigatePageDown;
	System::Byte FNavigateDown;
	int FDblClickDelay;
	
protected:
	DYNAMIC System::Classes::TPersistent* __fastcall GetOwner(void);
	
public:
	__fastcall TGLConsoleControls(System::Classes::TPersistent* AOwner);
	virtual void __fastcall Assign(System::Classes::TPersistent* Source);
	
__published:
	__property System::Byte NavigateUp = {read=FNavigateUp, write=FNavigateUp, default=36};
	__property System::Byte NavigateDown = {read=FNavigateDown, write=FNavigateDown, default=35};
	__property System::Byte NavigatePageUp = {read=FNavigatePageUp, write=FNavigatePageUp, default=33};
	__property System::Byte NavigatePageDown = {read=FNavigatePageDown, write=FNavigatePageDown, default=34};
	__property System::Byte NextCommand = {read=FNextCommand, write=FNextCommand, default=40};
	__property System::Byte PreviousCommand = {read=FPreviousCommand, write=FPreviousCommand, default=38};
	__property System::Byte AutoCompleteCommand = {read=FAutoCompleteCommand, write=FAutoCompleteCommand, default=17};
	__property int DblClickDelay = {read=FDblClickDelay, write=FDblClickDelay, default=300};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TGLConsoleControls(void) { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TGLCustomConsole : public Glscene::TGLBaseSceneObject
{
	typedef Glscene::TGLBaseSceneObject inherited;
	
private:
	Glhudobjects::TGLHUDSprite* FHudSprite;
	Glhudobjects::TGLHUDText* FHudText;
	Glwin32viewer::TGLSceneViewer* FSceneViewer;
	System::UnicodeString FInputLine;
	int FStartLine;
	int FCurrentCommand;
	int FPreviousTickCount;
	float FSize;
	System::Classes::TStringList* FColsoleLog;
	TGLConsoleCommandList* FCommands;
	TGLConsoleStringList* FAdditionalCommands;
	System::Classes::TStringList* FTypedCommands;
	TGLConsoleControls* FControls;
	TGLlConsoleEvent FOnCommandIssued;
	TGLConsoleOptions FOptions;
	System::UnicodeString FHint;
	void __fastcall SetSize(const float Value);
	void __fastcall SetSceneViewer(Glwin32viewer::TGLSceneViewer* const Value);
	Glbitmapfont::TGLCustomBitmapFont* __fastcall GetFont(void);
	void __fastcall SetFont(Glbitmapfont::TGLCustomBitmapFont* const Value);
	
protected:
	virtual void __fastcall DoOnCommandIssued(TGLUserInputCommand &UserInputCommand);
	virtual void __fastcall SetFontColor(const System::Uitypes::TColor Color);
	virtual System::Uitypes::TColor __fastcall GetFontColor(void);
	virtual void __fastcall SetHUDSpriteColor(const System::Uitypes::TColor Color);
	virtual System::Uitypes::TColor __fastcall GetHUDSpriteColor(void);
	virtual int __fastcall NumLines(void);
	virtual void __fastcall ShowConsoleHelp(void);
	virtual void __fastcall HandleUnknownCommand(const System::UnicodeString Command);
	virtual void __fastcall AutoCompleteCommand(void)/* overload */;
	void __fastcall AutoCompleteCommand(int &MatchCount, TGLConsoleMatchList &AdditionalCommandsMatchList, TGLConsoleMatchList &CommandsMatchList)/* overload */;
	virtual void __fastcall CommandIssued(TGLUserInputCommand &UserInputCommand);
	virtual void __fastcall FixCommand(TGLUserInputCommand &UserInputCommand);
	virtual TGLUserInputCommand __fastcall ParseString(System::UnicodeString str, System::UnicodeString caract);
	virtual void __fastcall ProcessInput(void);
	virtual void __fastcall RefreshHud(void);
	virtual void __fastcall RegisterBuiltInCommands(void);
	virtual void __fastcall ProcessInternalCommandHelp(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandClearScreen(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandConsoleHide(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandConsoleColor(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandConsoleRename(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandConsoleClearTypedCommands(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandSystemTime(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandSystemDate(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandViewerFPS(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandViewerResetPerformanceMonitor(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandViewerVSync(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall ProcessInternalCommandViewerAntiAliasing(TGLConsoleCommand* const ConsoleCommand, TGLCustomConsole* const Console, TGLUserInputCommand &Command);
	virtual void __fastcall GetHelpInternalCommandRename(System::TObject* Sender);
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall SetName(const System::Classes::TComponentName Value);
	
public:
	virtual void __fastcall ProcessKeyPress(const System::WideChar c);
	virtual void __fastcall ProcessKeyDown(const System::Word key);
	void __fastcall NavigateUp(void);
	void __fastcall NavigateDown(void);
	void __fastcall NavigatePageUp(void);
	void __fastcall NavigatePageDown(void);
	virtual void __fastcall RefreshHudSize(void);
	void __fastcall AddLine(const System::UnicodeString str);
	void __fastcall ClearTypedCommands(void);
	void __fastcall ExecuteCommand(const System::UnicodeString Command);
	void __fastcall ExecuteCommands(System::Classes::TStrings* const Commands);
	__fastcall virtual TGLCustomConsole(System::Classes::TComponent* AOwner);
	__fastcall virtual ~TGLCustomConsole(void);
	__property System::Uitypes::TColor FontColor = {read=GetFontColor, write=SetFontColor, stored=false, nodefault};
	__property System::Uitypes::TColor HUDSpriteColor = {read=GetHUDSpriteColor, write=SetHUDSpriteColor, stored=false, nodefault};
	__property System::UnicodeString InputLine = {read=FInputLine, write=FInputLine};
	__property System::Classes::TStringList* TypedCommands = {read=FTypedCommands};
	__property TGLConsoleCommandList* Commands = {read=FCommands};
	__property TGLConsoleStringList* AdditionalCommands = {read=FAdditionalCommands};
	__property TGLConsoleControls* Controls = {read=FControls};
	__property System::Classes::TStringList* ColsoleLog = {read=FColsoleLog};
	__property float Size = {read=FSize, write=SetSize};
	__property Glwin32viewer::TGLSceneViewer* SceneViewer = {read=FSceneViewer, write=SetSceneViewer};
	__property Glhudobjects::TGLHUDSprite* HudSprite = {read=FHudSprite};
	__property Glhudobjects::TGLHUDText* HudText = {read=FHudText};
	__property Glbitmapfont::TGLCustomBitmapFont* Font = {read=GetFont, write=SetFont, stored=false};
	__property TGLConsoleOptions Options = {read=FOptions, write=FOptions, nodefault};
	__property TGLlConsoleEvent OnCommandIssued = {read=FOnCommandIssued, write=FOnCommandIssued};
	__property System::UnicodeString Hint = {read=FHint, write=FHint};
	__property Visible = {default=0};
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLCustomConsole(Glscene::TGLBaseSceneObject* aParentOwner) : Glscene::TGLBaseSceneObject(aParentOwner) { }
	
};


class DELPHICLASS TGLConsole;
class PASCALIMPLEMENTATION TGLConsole : public TGLCustomConsole
{
	typedef TGLCustomConsole inherited;
	
__published:
	__property FontColor;
	__property HUDSpriteColor;
	__property InputLine = {default=0};
	__property TypedCommands;
	__property Commands;
	__property AdditionalCommands;
	__property Controls;
	__property ColsoleLog;
	__property SceneViewer;
	__property HudSprite;
	__property HudText;
	__property Font;
	__property Options;
	__property OnCommandIssued;
	__property Hint = {default=0};
	__property Tag = {default=0};
	__property ObjectsSorting = {default=0};
	__property Visible = {default=0};
	__property OnProgress;
public:
	/* TGLCustomConsole.Create */ inline __fastcall virtual TGLConsole(System::Classes::TComponent* AOwner) : TGLCustomConsole(AOwner) { }
	/* TGLCustomConsole.Destroy */ inline __fastcall virtual ~TGLConsole(void) { }
	
public:
	/* TGLBaseSceneObject.CreateAsChild */ inline __fastcall TGLConsole(Glscene::TGLBaseSceneObject* aParentOwner) : TGLCustomConsole(aParentOwner) { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 CONSOLE_MAX_COMMANDS = System::Int8(0x78);
}	/* namespace Glconsole */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLCONSOLE)
using namespace Glconsole;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlconsoleHPP
