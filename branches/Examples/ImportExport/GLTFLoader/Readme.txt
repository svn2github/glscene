GLTFLoader

glTF (GL Transmission Format) is an open project by Khronos providing a common, extensible format for 3D assets that is both efficient and highly interoperable with modern web technologies. 
The gltf-model component loads a 3D model using a glTF (.gltf or .glb) file.

Examples
----------------------------------------------
Khronos C# reference loader for glTF on github  - 
glTF-CSharp-Loader

It's as simple to use as Interface.LoadModel("PathToModel.gltf").  
You can use this loader in your project by importing the "glTF Loader" NuGet package.  
Additional examples can be found in the gltfLoaderUnitTests project.
To build the project, load the CSharp.sln solution.  
Click "Start". This will build glTFLoader_Shared, GeneratorLib, and Generator. 
It will then run the Generator. 
The generator reads the .spec files defining the glTF standard and generates a set of classes that will be used as the schema for loading your models. 
The glTFLoader project can now be built.  
You will need glTFLoader.dll and glTFLoader_shared.dll in order to use the loader in any subsequent project.
-------------------------------------------------------

GLTF Loader for Unreal Engine
https://github.com/RobertPoncelet/UnrealGLTFLoader

-------------------------------------------------------
GLTF Loader for Unity
https://github.com/robertlong/UnityGLTFLoader
Unity3D library for loading, parsing, and rendering assets stored in the GLTF 2.0 file format at runtime.

------------------------------------------------------------------


