// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLSCrossXML.pas' rev: 24.00 (Win32)

#ifndef GlscrossxmlHPP
#define GlscrossxmlHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.Classes.hpp>	// Pascal unit
#include <System.SysUtils.hpp>	// Pascal unit
#include <System.Variants.hpp>	// Pascal unit
#include <Xml.XMLIntf.hpp>	// Pascal unit
#include <Xml.XMLDoc.hpp>	// Pascal unit
#include <Xml.Xmldom.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Glscrossxml
{
//-- type declarations -------------------------------------------------------
typedef Xml::Xmlintf::_di_IXMLDocument GLSXMLDocument;

typedef Xml::Xmlintf::_di_IXMLNode GLSXMLNode;

typedef Xml::Xmldom::_di_IDOMNode GLSDOMNode;

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE Xml::Xmlintf::_di_IXMLDocument __fastcall GLSNewXMLDocument(void);
extern PACKAGE void __fastcall ReleaseXMLDocument(Xml::Xmlintf::_di_IXMLDocument &ADoc);
extern PACKAGE void __fastcall WriteXMLFile(Xml::Xmlintf::_di_IXMLDocument &ADoc, System::Classes::TStream* AStream)/* overload */;
extern PACKAGE void __fastcall ReadXMLFile(Xml::Xmlintf::_di_IXMLDocument &ADoc, System::Classes::TStream* AStream)/* overload */;
extern PACKAGE void __fastcall WriteXMLFile(Xml::Xmlintf::_di_IXMLDocument &ADoc, System::UnicodeString AFileName)/* overload */;
extern PACKAGE void __fastcall ReadXMLFile(Xml::Xmlintf::_di_IXMLDocument &ADoc, System::UnicodeString AFileName)/* overload */;
extern PACKAGE bool __fastcall GetXMLAttribute(const Xml::Xmlintf::_di_IXMLNode XMLNode, const System::UnicodeString AttrName, /* out */ System::UnicodeString &Value)/* overload */;
extern PACKAGE void __fastcall SetXMLAttribute(const Xml::Xmlintf::_di_IXMLNode XMLNode, const System::UnicodeString AttrName, const System::UnicodeString Value)/* overload */;
extern PACKAGE void __fastcall SetXMLAttribute(const Xml::Xmldom::_di_IDOMNode DOMNode, const System::UnicodeString AttrName, const System::UnicodeString Value)/* overload */;
extern PACKAGE bool __fastcall FindXMLNode(const Xml::Xmlintf::_di_IXMLNode ParentNode, const System::UnicodeString NodeName, /* out */ Xml::Xmlintf::_di_IXMLNode &ChildNode);
extern PACKAGE Xml::Xmldom::_di_IDOMNode __fastcall CreateDOMNode(const Xml::Xmldom::_di_IDOMNode ParentNode, const System::UnicodeString NodeName);
extern PACKAGE void __fastcall SetXMLText(const Xml::Xmldom::_di_IDOMNode DOMNode, const System::UnicodeString AText);
extern PACKAGE bool __fastcall GetXMLText(const Xml::Xmlintf::_di_IXMLNode XMLNode, /* out */ System::UnicodeString &AText);
extern PACKAGE int __fastcall GetXMLAttributeCount(const Xml::Xmlintf::_di_IXMLNode XMLNode);
extern PACKAGE Xml::Xmlintf::_di_IXMLNode __fastcall GetXMLAttribute(const Xml::Xmlintf::_di_IXMLNode XMLNode, int Idx)/* overload */;
}	/* namespace Glscrossxml */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLSCROSSXML)
using namespace Glscrossxml;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// GlscrossxmlHPP
