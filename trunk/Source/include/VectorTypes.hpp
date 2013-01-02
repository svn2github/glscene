// CodeGear C++Builder
// Copyright (c) 1995, 2012 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'VectorTypes.pas' rev: 24.00 (Win32)

#ifndef VectortypesHPP
#define VectortypesHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <GLCrossPlatform.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Vectortypes
{
//-- type declarations -------------------------------------------------------
struct DECLSPEC_DRECORD TVector2d
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			double X;
			double Y;
		};
		struct 
		{
			System::StaticArray<double, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector2f
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			float X;
			float Y;
		};
		struct 
		{
			System::StaticArray<float, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector2h
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			Glcrossplatform::THalfFloat X;
			Glcrossplatform::THalfFloat Y;
		};
		struct 
		{
			System::StaticArray<Glcrossplatform::THalfFloat, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector2i
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			int X;
			int Y;
		};
		struct 
		{
			System::StaticArray<int, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector2ui
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			unsigned X;
			unsigned Y;
		};
		struct 
		{
			System::StaticArray<unsigned, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector2s
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			short X;
			short Y;
		};
		struct 
		{
			System::StaticArray<short, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector2b
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Byte X;
			System::Byte Y;
		};
		struct 
		{
			System::StaticArray<System::Byte, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector2sb
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Int8 X;
			System::Int8 Y;
		};
		struct 
		{
			System::StaticArray<System::Int8, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector2e
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Extended X;
			unsigned:32;
			unsigned:16;
			System::Extended Y;
		};
		struct 
		{
			System::StaticArray<System::Extended, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector2w
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Word X;
			System::Word Y;
		};
		struct 
		{
			System::StaticArray<System::Word, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector2p
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			void *X;
			void *Y;
		};
		struct 
		{
			System::StaticArray<void *, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector3d
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			double X;
			double Y;
			double Z;
		};
		struct 
		{
			System::StaticArray<double, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector3f
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			float X;
			float Y;
			float Z;
		};
		struct 
		{
			System::StaticArray<float, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector3h
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			Glcrossplatform::THalfFloat X;
			Glcrossplatform::THalfFloat Y;
			Glcrossplatform::THalfFloat Z;
		};
		struct 
		{
			System::StaticArray<Glcrossplatform::THalfFloat, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector3i
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			int X;
			int Y;
			int Z;
		};
		struct 
		{
			System::StaticArray<int, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector3ui
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			unsigned X;
			unsigned Y;
			unsigned Z;
		};
		struct 
		{
			System::StaticArray<unsigned, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector3s
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			short X;
			short Y;
			short Z;
		};
		struct 
		{
			System::StaticArray<short, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector3b
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Byte X;
			System::Byte Y;
			System::Byte Z;
		};
		struct 
		{
			System::StaticArray<System::Byte, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector3sb
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Int8 X;
			System::Int8 Y;
			System::Int8 Z;
		};
		struct 
		{
			System::StaticArray<System::Int8, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector3e
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Extended X;
			unsigned:32;
			unsigned:16;
			System::Extended Y;
			unsigned:32;
			unsigned:16;
			System::Extended Z;
		};
		struct 
		{
			System::StaticArray<System::Extended, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector3w
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Word X;
			System::Word Y;
			System::Word Z;
		};
		struct 
		{
			System::StaticArray<System::Word, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector3p
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			void *X;
			void *Y;
			void *Z;
		};
		struct 
		{
			System::StaticArray<void *, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector4d
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			double X;
			double Y;
			double Z;
			double W;
		};
		struct 
		{
			System::StaticArray<double, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector4f
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			float X;
			float Y;
			float Z;
			float W;
		};
		struct 
		{
			System::StaticArray<float, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector4h
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			Glcrossplatform::THalfFloat X;
			Glcrossplatform::THalfFloat Y;
			Glcrossplatform::THalfFloat Z;
			Glcrossplatform::THalfFloat W;
		};
		struct 
		{
			System::StaticArray<Glcrossplatform::THalfFloat, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector4i
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			int X;
			int Y;
			int Z;
			int W;
		};
		struct 
		{
			System::StaticArray<int, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector4ui
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			unsigned X;
			unsigned Y;
			unsigned Z;
			unsigned W;
		};
		struct 
		{
			System::StaticArray<unsigned, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector4s
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			short X;
			short Y;
			short Z;
			short W;
		};
		struct 
		{
			System::StaticArray<short, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector4b
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Byte X;
			System::Byte Y;
			System::Byte Z;
			System::Byte W;
		};
		struct 
		{
			System::StaticArray<System::Byte, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector4sb
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Int8 X;
			System::Int8 Y;
			System::Int8 Z;
			System::Int8 W;
		};
		struct 
		{
			System::StaticArray<System::Int8, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector4e
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Extended X;
			unsigned:32;
			unsigned:16;
			System::Extended Y;
			unsigned:32;
			unsigned:16;
			System::Extended Z;
			unsigned:32;
			unsigned:16;
			System::Extended W;
		};
		struct 
		{
			System::StaticArray<System::Extended, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector4w
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			System::Word X;
			System::Word Y;
			System::Word Z;
			System::Word W;
		};
		struct 
		{
			System::StaticArray<System::Word, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TVector4p
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			void *X;
			void *Y;
			void *Z;
			void *W;
		};
		struct 
		{
			System::StaticArray<void *, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix2d
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector2d X;
			TVector2d Y;
		};
		struct 
		{
			System::StaticArray<TVector2d, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix2f
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector2f X;
			TVector2f Y;
		};
		struct 
		{
			System::StaticArray<TVector2f, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix2i
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector2i X;
			TVector2i Y;
		};
		struct 
		{
			System::StaticArray<TVector2i, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix2s
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector2s X;
			TVector2s Y;
		};
		struct 
		{
			System::StaticArray<TVector2s, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix2b
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector2b X;
			TVector2b Y;
		};
		struct 
		{
			System::StaticArray<TVector2b, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix2e
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector2e X;
			TVector2e Y;
		};
		struct 
		{
			System::StaticArray<TVector2e, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix2w
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector2w X;
			TVector2w Y;
		};
		struct 
		{
			System::StaticArray<TVector2w, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix2p
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector2p X;
			TVector2p Y;
		};
		struct 
		{
			System::StaticArray<TVector2p, 2> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix3d
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector3d X;
			TVector3d Y;
			TVector3d Z;
		};
		struct 
		{
			System::StaticArray<TVector3d, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix3f
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector3f X;
			TVector3f Y;
			TVector3f Z;
		};
		struct 
		{
			System::StaticArray<TVector3f, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix3i
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector3i X;
			TVector3i Y;
			TVector3i Z;
		};
		struct 
		{
			System::StaticArray<TVector3i, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix3s
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector3s X;
			TVector3s Y;
			TVector3s Z;
		};
		struct 
		{
			System::StaticArray<TVector3s, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix3b
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector3b X;
			TVector3b Y;
			TVector3b Z;
		};
		struct 
		{
			System::StaticArray<TVector3b, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix3e
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector3e X;
			TVector3e Y;
			TVector3e Z;
		};
		struct 
		{
			System::StaticArray<TVector3e, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix3w
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector3w X;
			TVector3w Y;
			TVector3w Z;
		};
		struct 
		{
			System::StaticArray<TVector3w, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix3p
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector3p X;
			TVector3p Y;
			TVector3p Z;
		};
		struct 
		{
			System::StaticArray<TVector3p, 3> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix4d
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector4d X;
			TVector4d Y;
			TVector4d Z;
			TVector4d W;
		};
		struct 
		{
			System::StaticArray<TVector4d, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix4f
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector4f X;
			TVector4f Y;
			TVector4f Z;
			TVector4f W;
		};
		struct 
		{
			System::StaticArray<TVector4f, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix4i
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector4i X;
			TVector4i Y;
			TVector4i Z;
			TVector4i W;
		};
		struct 
		{
			System::StaticArray<TVector4i, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix4s
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector4s X;
			TVector4s Y;
			TVector4s Z;
			TVector4s W;
		};
		struct 
		{
			System::StaticArray<TVector4s, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix4b
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector4b X;
			TVector4b Y;
			TVector4b Z;
			TVector4b W;
		};
		struct 
		{
			System::StaticArray<TVector4b, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix4e
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector4e X;
			TVector4e Y;
			TVector4e Z;
			TVector4e W;
		};
		struct 
		{
			System::StaticArray<TVector4e, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix4w
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector4w X;
			TVector4w Y;
			TVector4w Z;
			TVector4w W;
		};
		struct 
		{
			System::StaticArray<TVector4w, 4> V;
		};
		
	};
	#pragma pack(pop)
};


struct DECLSPEC_DRECORD TMatrix4p
{
	#pragma pack(push,1)
	union
	{
		struct 
		{
			TVector4p X;
			TVector4p Y;
			TVector4p Z;
			TVector4p W;
		};
		struct 
		{
			System::StaticArray<TVector4p, 4> V;
		};
		
	};
	#pragma pack(pop)
};


#pragma pack(push,1)
struct DECLSPEC_DRECORD TD3DVector
{
	union
	{
		struct 
		{
			TVector3f V;
		};
		struct 
		{
			float X;
			float Y;
			float Z;
		};
		
	};
};
#pragma pack(pop)


#pragma pack(push,1)
struct DECLSPEC_DRECORD TD3DMatrix
{
	union
	{
		struct 
		{
			TMatrix4f M;
		};
		struct 
		{
			float _11;
			float _12;
			float _13;
			float _14;
			float _21;
			float _22;
			float _23;
			float _24;
			float _31;
			float _32;
			float _33;
			float _34;
			float _41;
			float _42;
			float _43;
			float _44;
		};
		
	};
};
#pragma pack(pop)


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Vectortypes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_VECTORTYPES)
using namespace Vectortypes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// VectortypesHPP
