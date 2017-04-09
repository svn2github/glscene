{$WARN SYMBOL_PLATFORM OFF}
{.$define DEBUG_LAYERS ON}

(* Copyright (c) 2016 Oleksii Karpeniuk (Fantom)

  vulkan_app <p>

      Base classs for Vulkan initialization

	<b>Historique : </b><font size=-1><ul>
      <li>04/13/16 - Fantom - Created
	</ul></font>

*)

unit vulkan_app;

interface

uses winapi.windows, winapi.messages, Vulkan, SysUtils, Generics.Collections;

const APP_SHORT_NAME = 'Vulkan test';
const APP_LONG_NAME = 'Vulkan initialization test';

type
  TExtentionsList = TList<PAnsiChar>;
  TLayersList = TList<PAnsiChar>;
  TExtensionsPropertyList = TList<TVkExtensionProperties>;
  TLayersPropertyList = TList<TVkLayerProperties>;
  TPhysicalDeviceList = TList<TVkPhysicalDevice>;
  TImageList = TList<TVkImage>;
  TImageViewList = TList<TVkImageView>;
  TCommandPoolList = TList<TVkCommandPool>;


  TQueueType = (qtGraphics, qtCompute, qtTransfer, qtSparseBinding,
    qtSurfaceSupport, qtPresentationSupport);
  TQueueTypes = set of TQueueType;

  TQueueFamilyProperties = record
    QueueIndex: cardinal;
    Prop: TVkQueueFamilyProperties;
    QueueTypes: TQueueTypes;
  end;

  TQueueFamilyList = TList<TQueueFamilyProperties>;

  TDeviceSurface = record
    surface: TVkSurfaceKHR;
    capabilities: TVkSurfaceCapabilitiesKHR;
    formats: array of TVkSurfaceFormatKHR;
    modes: array of TVkPresentModeKHR;
  end;

  TDeviceProperties = class;

  TImageInfo = record
    image: TVkImage;
    view: TVkImageView;
    format: TVkFormat;
  end;
  TImageInfoList = TList<TImageInfo>;

  TSwapChain = class
  private
    FSwapChain: TVkSwapchainKHR;
    FSWC_info: TVkSwapchainCreateInfoKHR;
    Fdev: TVkDevice;
    Fdev_prop: TDeviceProperties;
    FImageInfos: TImageInfoList;
    procedure InitializeImageViews;
    procedure ClearImageViews;
  public
    constructor Create(dev: TVkDevice; dev_prop: TDeviceProperties);
    destructor Destroy; override;
    procedure Initialize(extents: PVkExtent2D = nil; VSync: boolean = true;
      BuffersCount: cardinal = 0);

    property Swapchain: TVkSwapchainKHR read FSwapChain;
    property Images: TImageInfoList read FImageInfos;

  end;

  TDeviceProperties = class
  private
    FDevice: TVkPhysicalDevice;
    FInfo: TVkPhysicalDeviceProperties;
    FLayers: TLayersPropertyList;
    FExtentions: TExtensionsPropertyList;
    FFamilies: TQueueFamilyList;
    FSurface: TDeviceSurface;
    FGraphicsQueueIndex: cardinal;
    FPresenterQueueIndex: cardinal;

    function isDevSuppPres: boolean;
    function GetSurfaceFamilyQueue: cardinal;
    function GetGraphicFamilyQueue: cardinal;
    function GetCompatibleQueue: cardinal;
  public
    property Device: TVkPhysicalDevice read Fdevice;
    property Info: TVkPhysicalDeviceProperties read FInfo;
    property Layers: TLayersPropertyList read FLayers;
    property Extentions: TExtensionsPropertyList read FExtentions;
    property Families: TQueueFamilyList read FFamilies;
    property Surface: TDeviceSurface read FSurface;
    property isDeviceSupportPresenting: boolean read isDevSuppPres;

    property GraphicsQueueIndex: cardinal read FGraphicsQueueIndex;
    property PresenterQueueIndex: cardinal read FPresenterQueueIndex;

    function GetFamilyIndex(bits: TVkQueueFlags): cardinal;
    function CreatePresenter: TVkDevice;
    function InitSwapChainInfo(old_swapchain: TVkSwapchainKHR = VK_NULL_HANDLE;
      extents: PVkExtent2D = nil; VSync: boolean = true; BuffersCount: cardinal = 0): TVkSwapchainCreateInfoKHR;
    constructor Create(dev: TVkPhysicalDevice; aSurface: TVkSurfaceKHR);
    destructor Destroy; override;
  end;

  TDevicePropertyList = TObjectList<TDeviceProperties>;

  TCommandBuffers = array of TVkCommandBuffer;

  TDeviceContext = class
  private
    FDev: TVkDevice;
    FCommandPools: TCommandPoolList;
  public
    constructor Create(dev: TVkDevice);
    destructor Destroy; override;

    function CreateCommandPool(queueFamilyIndex: cardinal; resetable: boolean; shortTerms: boolean): TVkCommandPool;
    function AllocateCommandBuffers(pool: TVkCommandPool; level: TVkCommandBufferLevel;
      commandBufferCount: cardinal): TCommandBuffers;
    function AllocateCommandBuffer(pool: TVkCommandPool; level: TVkCommandBufferLevel): TVkCommandBuffer;
    procedure ResetCommandBuffer(buffer: TVkCommandBuffer; flags: TVkCommandBufferResetFlags);

    property CommandPools: TCommandPoolList read FCommandPools;
    property handle: TVkDevice read FDev;
  end;

TKeypressedProc = procedure (key: cardinal) of object;

TWin32Window = class
private
  wc: TWndClass;
  hWindow: HWND;
	Fprepared: boolean;
  FWidth,FHeight: integer;
  FTitle, FName: string;
  FonKeypressed: TKeypressedProc;
  procedure Keypressed(key: cardinal);

public
  constructor Create(width, height: integer; title, name: string; MainWndProc: TFNWndProc);
  procedure handleMessages(hWindow: HWND; Msg: UINT; wParam: wParam; lParam: lParam); stdcall;

  property onKeyressed: TKeypressedProc read FOnKeypressed write FOnKeypressed;
  property Window: HWND read hWindow;
  property Instance: HINST read wc.hInstance;
end;

TWSI = class
private
  FWnd: TWin32Window;
  FInst: TVkInstance;
  FSurface: TVkSurfaceKHR;
public
  constructor Create(inst: TVkInstance; wnd: TWin32Window);
  destructor Destroy; override;
  property Surface: TVkSurfaceKHR read FSurface;
end;

TVulkanInstance = class
private
  FWindow: TWin32Window;
  FWSI: TWSI;
  Finst: TVkInstance;
  FAvailableLayers: TLayersPropertyList;
  FAvailableExtentions: TExtensionsPropertyList;
  FPhysicalDevices: TPhysicalDeviceList;
  FDeviceProperties: TDevicePropertyList;
  procedure CreateVulkanInstance(ext: TExtentionsList=nil; layers: TLayersList=nil);
  procedure RequestLayersAndExt;
  procedure RequestGPUs;
public
	constructor Create(window: TWin32Window; ext: TExtentionsList=nil; layers: TLayersList=nil);
  destructor Destroy; override;

  property Instance: TVkInstance read FInst;
  property Extentions: TExtensionsPropertyList read FAvailableExtentions;
  property Layers: TLayersPropertyList read FAvailableLayers;
  property Devices: TDevicePropertyList read FDeviceProperties;

  property WSI: TWSI read FWSI;

end;

function VulkanErrors(err: TVkResult): string;
procedure LOG(msg: string); overload;
procedure LOG(err: TVkResult; msg: string=''); overload;

implementation

var res: TVkResult = VK_SUCCESS;

procedure LOG(err: TVkResult; msg: string); overload;
begin
  if msg<>'' then writeln(VulkanErrors(err), ': ', msg)
  else writeln(VulkanErrors(err));
end;

procedure LOG(msg: string); overload;
begin
  writeln(msg);
end;

function VulkanErrors(err: TVkResult): string;
begin
		case err of
      VK_SUCCESS: Result := 'VK_SUCCESS';
      VK_NOT_READY: Result := 'VK_NOT_READY';
      VK_TIMEOUT: Result := 'VK_TIMEOUT';
      VK_EVENT_SET: Result := 'VK_EVENT_SET';
      VK_EVENT_RESET: Result := 'VK_EVENT_RESET';
      VK_INCOMPLETE: Result := 'VK_INCOMPLETE';
      VK_ERROR_OUT_OF_HOST_MEMORY: Result := 'VK_ERROR_OUT_OF_HOST_MEMORY';
      VK_ERROR_OUT_OF_DEVICE_MEMORY: Result := 'VK_ERROR_OUT_OF_DEVICE_MEMORY';
      VK_ERROR_INITIALIZATION_FAILED: Result := 'VK_ERROR_INITIALIZATION_FAILED';
      VK_ERROR_DEVICE_LOST: Result := 'VK_ERROR_DEVICE_LOST';
      VK_ERROR_MEMORY_MAP_FAILED: Result := 'VK_ERROR_MEMORY_MAP_FAILED';
      VK_ERROR_LAYER_NOT_PRESENT: Result := 'VK_ERROR_LAYER_NOT_PRESENT';
      VK_ERROR_EXTENSION_NOT_PRESENT: Result := 'VK_ERROR_EXTENSION_NOT_PRESENT';
      VK_ERROR_FEATURE_NOT_PRESENT: Result := 'VK_ERROR_FEATURE_NOT_PRESENT';
      VK_ERROR_INCOMPATIBLE_DRIVER: Result := 'VK_ERROR_INCOMPATIBLE_DRIVER';
      VK_ERROR_TOO_MANY_OBJECTS: Result := 'VK_ERROR_TOO_MANY_OBJECTS';
      VK_ERROR_FORMAT_NOT_SUPPORTED: Result := 'VK_ERROR_FORMAT_NOT_SUPPORTED';
      VK_ERROR_SURFACE_LOST_KHR: Result := 'VK_ERROR_SURFACE_LOST_KHR';
      VK_ERROR_NATIVE_WINDOW_IN_USE_KHR: Result := 'VK_ERROR_NATIVE_WINDOW_IN_USE_KHR';
      VK_SUBOPTIMAL_KHR: Result := 'VK_SUBOPTIMAL_KHR';
      VK_ERROR_OUT_OF_DATE_KHR: Result := 'VK_ERROR_OUT_OF_DATE_KHR';
      VK_ERROR_INCOMPATIBLE_DISPLAY_KHR: Result := 'VK_ERROR_INCOMPATIBLE_DISPLAY_KHR';
      VK_ERROR_VALIDATION_FAILED_EXT: Result := 'VK_ERROR_VALIDATION_FAILED_EXT';
      VK_ERROR_INVALID_SHADER_NV: Result := 'VK_ERROR_INVALID_SHADER_NV';
//      VK_ERROR_INVALID_PARAMETER_NV: Result := 'VK_ERROR_INVALID_PARAMETER_NV';
//      VK_ERROR_INVALID_ALIGNMENT_NV: Result := 'VK_ERROR_INVALID_ALIGNMENT_NV';
      else Result := 'Unknown error code detected';
    end;
end;


{ TWin32Window }

constructor TWin32Window.Create(width, height: integer; title, name: string; MainWndProc: TFNWndProc);
begin
  FWidth := width;
  FHeight := height;
  FTitle := title;
  FName := name;

  wc.lpszClassName := 'VulkanAppClass';
  wc.lpfnWndProc   := MainWndProc;
  wc.Style         := CS_VREDRAW or CS_HREDRAW;
  wc.hInstance     := hInstance;
  wc.hIcon         := LoadIcon(0, IDI_APPLICATION);
  wc.hCursor       := LoadCursor(0, IDC_ARROW);
  wc.hbrBackground := (COLOR_WINDOW + 1);
  wc.lpszMenuName  := nil;
  wc.cbClsExtra    := 0;
  wc.cbWndExtra    := 0;
  winapi.windows.RegisterClass(wc);
  hWindow := CreateWindowEx(WS_EX_APPWINDOW or WS_EX_WINDOWEDGE,
    PChar(wc.lpszClassName),PChar(''),
    WS_VISIBLE or WS_CLIPSIBLINGS or WS_CLIPCHILDREN or WS_OVERLAPPEDWINDOW,
    CW_USEDEFAULT, CW_USEDEFAULT,
    width, height, 0,0,
    hInstance, nil);

  ShowWindow(hWindow, CmdShow);
  UpDateWindow(hWindow);
	SetForegroundWindow(hWindow);
	SetFocus(hWindow);
  Fprepared := true;
end;

procedure TWin32Window.handleMessages(hWindow: HWND; Msg: UINT; wParam: wParam;
  lParam: lParam); stdcall;
begin
  case Msg of
    WM_CLOSE: begin
      Fprepared := false;
      DestroyWindow(hWindow);
      PostQuitMessage(0);
    end;
    WM_PAINT:	ValidateRect(hWindow, nil);
    WM_KEYDOWN: begin
      case wParam of
        VK_ESCAPE: begin
          Fprepared := false;
          DestroyWindow(hWindow);
          PostQuitMessage(0);
        end;
      end;
   		KeyPressed(cardinal(wParam));
    end;
	  WM_RBUTTONDOWN:;
	  WM_LBUTTONDOWN:;
    WM_MOUSEMOVE:;
  end;
end;

procedure TWin32Window.Keypressed(key: cardinal);
begin
  if assigned(FonKeypressed) then FonKeypressed(key);
end;

{ TVulkanApp }

constructor TVulkanInstance.Create(window: TWin32Window; ext: TExtentionsList; layers: TLayersList);
begin
  FWindow := window;
  Finst := VK_NULL_HANDLE;
  CreateVulkanInstance(ext, layers);
  FWSI := TWSI.Create(Finst, window);

  FAvailableLayers := TLayersPropertyList.Create;
  FAvailableExtentions := TExtensionsPropertyList.Create;
  RequestLayersAndExt;

  FPhysicalDevices := TPhysicalDeviceList.Create;
  FDeviceProperties := TDevicePropertyList.Create(true);
  RequestGPUs;

end;

procedure TVulkanInstance.CreateVulkanInstance(ext: TExtentionsList=nil; layers: TLayersList=nil);
var
	app: TVkApplicationInfo;
  inst_info: TVkInstanceCreateInfo;
begin
  app.sType := VK_STRUCTURE_TYPE_APPLICATION_INFO;
  app.pNext := nil;
  app.pApplicationName := APP_SHORT_NAME;
  app.applicationVersion := 0;
  app.pEngineName := APP_SHORT_NAME;
  app.engineVersion := 0;
  app.apiVersion := VK_MAKE_VERSION(1, 0, 0);

	inst_info.sType := VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  inst_info.pNext := nil;
  inst_info.pApplicationInfo := @app;
  if assigned(layers) then begin
    inst_info.enabledLayerCount := layers.count;
    inst_info.ppEnabledLayerNames := @layers.list[0];
  end else begin
    inst_info.enabledLayerCount := 0;
    inst_info.ppEnabledLayerNames := nil;
  end;

  if assigned(ext) then begin
    inst_info.enabledExtensionCount := ext.count;
    inst_info.ppEnabledExtensionNames := @ext.list[0];
  end else begin
    inst_info.enabledExtensionCount := 0;
    inst_info.ppEnabledExtensionNames := nil;
  end;

  res := vkCreateInstance(@inst_info, nil, @Finst);
  if(res <> VK_SUCCESS) then begin
    LOG(res, 'Instance Creation failed');
    readln; halt(0);
  end;

end;

destructor TVulkanInstance.Destroy;
begin
  if Finst <> VK_NULL_HANDLE then begin
    FAvailableLayers.Free;
    FAvailableExtentions.Free;
    FPhysicalDevices.Free;
    FDeviceProperties.Free;
    FWSI.Free;
    vkDestroyInstance(Finst, nil);
  end;
  inherited;
end;

procedure TVulkanInstance.RequestGPUs;
var gpu_count: cardinal;
    i: integer;
begin
  res := vkEnumeratePhysicalDevices(FInst, @gpu_count, nil);
  if(res <> VK_SUCCESS) or (gpu_count = 0) then begin
    LOG(res, 'Vulkan compatible GPU not found');
    readln; halt(0);
  end;
  FPhysicalDevices.Count := gpu_count;
  res := vkEnumeratePhysicalDevices(Finst, @gpu_count, @FPhysicalDevices.List[0]);
  if(res <> VK_SUCCESS) then LOG(res, 'GPU enumeration failed');
  for i:=0 to gpu_count-1 do
    FDeviceProperties.Add(TDeviceProperties.Create(FPhysicalDevices[i], FWSI.Surface));
end;

procedure TVulkanInstance.RequestLayersAndExt;
var
  count: cardinal;
begin
		res := vkEnumerateInstanceExtensionProperties(nil, @count, nil);
    if(res <> VK_SUCCESS) then LOG(res, 'Instance Extensions enumeration failed')
		else if (count>0) then begin
      FAvailableExtentions.Count:=count;
			res := vkEnumerateInstanceExtensionProperties(nil, @count, @FAvailableExtentions.List[0]);
      if(res <> VK_SUCCESS) then LOG(res, 'Instance Extensions enumeration failed');
		end;
		res := vkEnumerateInstanceLayerProperties(@count, nil);
		if(res <> VK_SUCCESS) then LOG(res, 'Instance Layers enumeration failed')
		else if (count>0) then begin
				FAvailableLayers.Count:=count;
				res := vkEnumerateInstanceLayerProperties(@count, @FAvailableLayers.List[0]);
				if(res <> VK_SUCCESS) then LOG(res, 'Instance Layers enumeration failed');
		end;
end;

{ WSI }

constructor TWSI.Create(inst: TVkInstance; wnd: TWin32Window);
var surfCreateInfo: TVkWin32SurfaceCreateInfoKHR;
begin
  FWnd:=wnd;
  FInst := inst;
  surfCreateInfo.sType := VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR;
  surfCreateInfo.pNext := nil;
  surfCreateInfo.flags := 0;
  surfCreateInfo.hinstance_ := FWnd.Instance;
  surfCreateInfo.hwnd_ := FWnd.Window;
  res := vkCreateWin32SurfaceKHR(FInst, @surfCreateInfo, nil, @FSurface);
  if(res <> VK_SUCCESS) then begin
    LOG(res, 'Win32 Surface Creation failed');
    readln; halt(0);
  end;
end;

destructor TWSI.Destroy;
begin
  vkDestroySurfaceKHR(FInst, FSurface, nil);
  inherited;
end;

{ TDeviceProperties }

constructor TDeviceProperties.Create(dev: TVkPhysicalDevice; aSurface: TVkSurfaceKHR);
var count: cardinal;
    cap: TVkSurfaceCapabilitiesKHR;
    queues: array of TVkQueueFamilyProperties;
    descr: TQueueFamilyProperties;
    i: integer;
    supported: boolean;
begin
  FDevice := dev;
  FSurface.surface := aSurface;

  Flayers := TLayersPropertyList.Create;
  FExtentions := TExtensionsPropertyList.Create;
  FFamilies := TQueueFamilyList.Create;

  FGraphicsQueueIndex := MAXLONG;
  FPresenterQueueIndex := MAXLONG;

  vkGetPhysicalDeviceProperties(dev, @Finfo);
	res := vkEnumerateDeviceLayerProperties(dev, @count, nil);
	if(res <> VK_SUCCESS) then LOG(res, 'Device Layers enumeration failed');
  Flayers.Count := count;
	res := vkEnumerateDeviceLayerProperties(dev, @count, @Flayers.List[0]);
  //Requested only global extentions, TODO: assign extentions to layers
  res := vkEnumerateDeviceExtensionProperties(dev, nil, @count, nil);
	if(res <> VK_SUCCESS) then LOG(res, 'Device Extentions enumeration failed');
  FExtentions.Count := count;
	res := vkEnumerateDeviceExtensionProperties(dev, nil, @count, @FExtentions.List[0]);

	vkGetPhysicalDeviceQueueFamilyProperties(dev, @count, nil);
  setlength(queues, count);
	vkGetPhysicalDeviceQueueFamilyProperties(dev, @count, @queues[0]);

  for i := 0 to count-1 do begin
    descr.QueueIndex := i;
    descr.Prop := queues[i];
    descr.QueueTypes := [];
    if (queues[i].queueFlags and ord(VK_QUEUE_GRAPHICS_BIT)) <> 0
    then include(descr.QueueTypes, qtGraphics);

    if (queues[i].queueFlags and ord(VK_QUEUE_COMPUTE_BIT)) <> 0
    then include(descr.QueueTypes, qtCompute);

    if (queues[i].queueFlags and ord(VK_QUEUE_TRANSFER_BIT)) <> 0
    then include(descr.QueueTypes, qtTransfer);

    if (queues[i].queueFlags and ord(VK_QUEUE_SPARSE_BINDING_BIT)) <> 0
    then include(descr.QueueTypes, qtSparseBinding);

    supported := false;
    res := vkGetPhysicalDeviceSurfaceSupportKHR(dev, i, aSurface, @supported);
    if(res <> VK_SUCCESS) then LOG(res, 'Getting Device Surface Support for queue failed');
    if supported then include(descr.QueueTypes, qtSurfaceSupport);

    supported := boolean(vkGetPhysicalDeviceWin32PresentationSupportKHR(dev, i));
    if supported then include(descr.QueueTypes, qtPresentationSupport);

    FFamilies.Add(descr);
  end;

  res := vkGetPhysicalDeviceSurfaceCapabilitiesKHR(dev, aSurface, @cap);
  FSurface.capabilities := cap;
  if (res <> VK_SUCCESS) then LOG(res, 'Getting Device Surface Capabilities failed');

  res := vkGetPhysicalDeviceSurfaceFormatsKHR(dev, aSurface, @count, nil);
	if (res <> VK_SUCCESS) or (count = 0) then LOG(res, 'Getting Surface Formats failed')
  else begin
    setlength(FSurface.formats, count);
    res := vkGetPhysicalDeviceSurfaceFormatsKHR(dev, aSurface, @count, @FSurface.formats[0]);
    if (res <> VK_SUCCESS) then LOG(res, 'Getting Surface Formats failed');
  end;

  res := vkGetPhysicalDeviceSurfacePresentModesKHR(dev, aSurface, @count, nil);
	if (res <> VK_SUCCESS) or (count = 0) then LOG(res, 'Getting Surface Present Modes failed')
  else begin
    setlength(FSurface.modes, count);
    res := vkGetPhysicalDeviceSurfacePresentModesKHR(dev, aSurface, @count, @FSurface.modes[0]);
  	if (res <> VK_SUCCESS) or (count = 0) then LOG(res, 'Getting Surface Present Modes failed')
  end;

  FPresenterQueueIndex := GetCompatibleQueue;
  if FPresenterQueueIndex <> MAXLONG then FGraphicsQueueIndex := FPresenterQueueIndex
  else begin
    FPresenterQueueIndex := GetSurfaceFamilyQueue;
    FGraphicsQueueIndex := GetGraphicFamilyQueue;
  end;
end;

function TDeviceProperties.CreatePresenter: TVkDevice;
var
  count: cardinal;
  infos: array [0..1] of TVkDeviceQueueCreateInfo;
  dev_info: TVkDeviceCreateInfo;
  dev: TVkDevice;
const
  queuePriorities: TVkFloat = 1.0;
  extensionNames: array[0..1] of PVkChar=(VK_KHR_SWAPCHAIN_EXTENSION_NAME, '');
  layersNames: array[0..1] of PVkChar = (
    'VK_LAYER_LUNARG_swapchain',
    'VK_LAYER_LUNARG_standard_validation'
  );
begin
  infos[0].sType := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
  infos[0].pNext := nil;
  infos[0].flags := 0;
  infos[0].queueFamilyIndex := FGraphicsQueueIndex;
  infos[0].queueCount := 1;
  infos[0].pQueuePriorities := @queuePriorities;

  infos[1].sType := VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
  infos[1].pNext := nil;
  infos[1].flags := 0;
  infos[1].queueFamilyIndex := FPresenterQueueIndex;
  infos[1].queueCount := 1;
  infos[1].pQueuePriorities := @queuePriorities;

  if FPresenterQueueIndex <> FGraphicsQueueIndex then count := 2 else count := 1;

  dev_info.sType := VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  dev_info.pNext := nil;
  dev_info.flags := 0;
  dev_info.queueCreateInfoCount := count;
  dev_info.pQueueCreateInfos := @infos[0];
  {$ifdef DEBUG_LAYERS}
  dev_info.enabledLayerCount := 2;
  dev_info.ppEnabledLayerNames := PPVkChar(pointer(@layersNames));
  {$else}
  dev_info.enabledLayerCount := 0;
  dev_info.ppEnabledLayerNames := nil;
  {$endif}

  dev_info.enabledExtensionCount := 1;
  dev_info.ppEnabledExtensionNames := PPVkChar(pointer(@extensionNames));
  dev_info.pEnabledFeatures := nil;

  dev := VK_NULL_HANDLE;
  res := vkCreateDevice( FDevice, @dev_info, nil, @dev);
	if (res <> VK_SUCCESS) then LOG(res, 'Could not create device for presenting');
  result := dev;
end;

destructor TDeviceProperties.Destroy;
begin
  Flayers.Free;
  FExtentions.Free;
  FFamilies.Free;

  inherited;
end;

function TDeviceProperties.GetCompatibleQueue: cardinal;
var i: integer;
begin
  for i:=0 to FFamilies.Count-1 do begin
    if  (qtSurfaceSupport in FFamilies[i].QueueTypes)
    and (qtGraphics in FFamilies[i].QueueTypes) then begin
      result := FFamilies[i].QueueIndex;
      exit;
    end;
  end;
  result := MAXLONG;
end;

function TDeviceProperties.GetFamilyIndex(bits: TVkQueueFlags): cardinal;
var i: integer;
begin
  for i:=0 to FFamilies.Count-1 do begin
    if FFamilies[i].Prop.queueFlags and bits <> 0 then begin
      result := i; exit;
    end;
  end;
  result := MAXLONG;
end;


function TDeviceProperties.GetGraphicFamilyQueue: cardinal;
var i: integer;
begin
  for i:=0 to FFamilies.Count-1 do begin
    if (qtGraphics in FFamilies[i].QueueTypes) then begin
      result := FFamilies[i].QueueIndex;
      exit;
    end;
  end;
  result := MAXLONG;
end;

function TDeviceProperties.GetSurfaceFamilyQueue: cardinal;
var i: integer;
begin
  for i:=0 to FFamilies.Count-1 do begin
    if  (qtSurfaceSupport in FFamilies[i].QueueTypes) then begin
      result := FFamilies[i].QueueIndex;
      exit;
    end;
  end;
  result := MAXLONG;
end;

function TDeviceProperties.InitSwapChainInfo(old_swapchain: TVkSwapchainKHR;
      extents: PVkExtent2D; VSync: boolean; BuffersCount: cardinal): TVkSwapchainCreateInfoKHR;
var
  i,j, idx,optimal: integer;
const
  modes: array[0..2] of TVkPresentModeKHR = ( VK_PRESENT_MODE_FIFO_RELAXED_KHR,
    VK_PRESENT_MODE_IMMEDIATE_KHR, VK_PRESENT_MODE_MAILBOX_KHR );
begin

  Result.sType := VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR;
  Result.pNext := nil;
  Result.flags := 0;
  Result.surface := FSurface.surface;
  Result.imageArrayLayers := 1;
  Result.imageUsage := ord(VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT);
  Result.imageSharingMode := VK_SHARING_MODE_EXCLUSIVE;
  Result.queueFamilyIndexCount := 0;
  Result.pQueueFamilyIndices := nil;
  Result.preTransform := VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR;
  Result.compositeAlpha := VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR;
  Result.clipped := VK_TRUE;
  Result.oldSwapchain := old_swapchain;

  if assigned(extents) then
    Result.imageExtent := extents^
  else
    Result.imageExtent := FSurface.capabilities.currentExtent;

  if BuffersCount<>0 then
    Result.minImageCount := BuffersCount
  else
    Result.minImageCount := FSurface.capabilities.maxImageCount;

  if FSurface.formats[0].format <> VK_FORMAT_UNDEFINED then begin
    Result.imageFormat := FSurface.formats[0].format;
    Result.imageColorSpace := FSurface.formats[0].colorSpace;
  end else begin
    Result.imageFormat := VK_FORMAT_B8G8R8A8_UNORM;
    Result.imageColorSpace := VK_COLORSPACE_SRGB_NONLINEAR_KHR;
  end;

  Result.presentMode := VK_PRESENT_MODE_FIFO_KHR;
  if not VSync then begin
    optimal := -1;
    for i:=0 to length(FSurface.modes)-1 do begin
      idx := -1;
      for j := 0 to 2 do if FSurface.modes[i] = modes[j] then idx := j;
      if (idx > optimal) then optimal := idx;
    end;
    if optimal <> -1 then
      Result.presentMode := modes[optimal];
  end;
end;

function TDeviceProperties.isDevSuppPres: boolean;
var i: integer;
begin
  for i:=0 to FFamilies.Count-1 do begin
    if  (qtPresentationSupport in FFamilies[i].QueueTypes) then begin
      result := true;
      exit;
    end;
  end;
  result := false;
end;

{ TSwapChain }

procedure TSwapChain.ClearImageViews;
var i: integer;
begin
  for i := 0 to FImageInfos.Count-1 do begin
     vkDestroyImageView(FDev, FImageInfos[i].view, nil);
  end;
  FImageInfos.Clear;
end;

constructor TSwapChain.Create(dev: TVkDevice; dev_prop: TDeviceProperties);
begin
  assert(not(not assigned(dev_prop) or (dev_prop.Device = VK_NULL_HANDLE)),
    'Can''t create Swapchain object, incorrect parameters');
  FDev := dev;
  Fdev_prop := dev_prop;
  FSwapChain := VK_NULL_HANDLE;
  FImageInfos := TImageInfoList.Create;
end;

procedure TSwapChain.Initialize(extents: PVkExtent2D; VSync: boolean;
      BuffersCount: cardinal);
var old_swapchain: TVkSwapchainKHR;
begin
  ClearImageViews;
  old_swapchain := FSwapChain;

  FSWC_info := Fdev_prop.InitSwapChainInfo(FSwapChain, extents, VSync, BuffersCount);
  res := vkCreateSwapchainKHR(Fdev, @FSWC_info, nil, @FSwapChain);
  if (res <> VK_SUCCESS) then LOG(res, 'Creating Swapchain failed');

  if old_swapchain<>VK_NULL_HANDLE then
    vkDestroySwapchainKHR(FDev, old_swapchain, nil);
  InitializeImageViews;
end;

destructor TSwapChain.Destroy;
begin
  ClearImageViews;
  if FSwapChain<>VK_NULL_HANDLE then
    vkDestroySwapchainKHR(FDev, Fswapchain, nil);
  FImageInfos.Free;
  inherited;
end;

procedure TSwapChain.InitializeImageViews;
var
  Images: array of TVkImage;
  count: cardinal;
  i: integer;
  img_info: TImageInfo;
  image_view_info: TVkImageViewCreateInfo;
begin
	res := vkGetSwapchainImagesKHR(FDev, FSwapchain, @count, nil);
 	if (res <> VK_SUCCESS) then LOG(res, 'Getting Swapchain Image Count failed');
  if count = 0 then exit;

  setlength(Images, count);
	res := vkGetSwapchainImagesKHR(FDev, FSwapchain, @count, @Images[0]);
  if (res <> VK_SUCCESS) then LOG(res, 'Getting Swapchain Images failed');

  for i := 0 to count-1 do begin
    img_info.image := Images[i];
    img_info.format := FSWC_info.imageFormat;
    image_view_info.sType := VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO;
    image_view_info.pNext := nil;
    image_view_info.format := FSWC_info.imageFormat;
    image_view_info.components.r := VK_COMPONENT_SWIZZLE_R;
    image_view_info.components.g := VK_COMPONENT_SWIZZLE_G;
    image_view_info.components.b := VK_COMPONENT_SWIZZLE_B;
    image_view_info.components.a := VK_COMPONENT_SWIZZLE_A;
    image_view_info.subresourceRange.aspectMask := ord(VK_IMAGE_ASPECT_COLOR_BIT);
    image_view_info.subresourceRange.baseMipLevel := 0;
    image_view_info.subresourceRange.levelCount := 1;
    image_view_info.subresourceRange.baseArrayLayer := 0;
    image_view_info.subresourceRange.layerCount := 1;
    image_view_info.viewType := VK_IMAGE_VIEW_TYPE_2D;
    image_view_info.flags := 0;
    image_view_info.image := img_info.image;
    res := vkCreateImageView(Fdev, @image_view_info, nil, @img_info.view);
    if (res <> VK_SUCCESS) then LOG(res, 'Creating Image Views failed');
    FImageInfos.Add(img_info);
  end;
end;

{ TDeviceContext }

function TDeviceContext.AllocateCommandBuffer(pool: TVkCommandPool;
  level: TVkCommandBufferLevel): TVkCommandBuffer;
var allocateInfo: TVkCommandBufferAllocateInfo;
begin
  allocateInfo.sType := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  allocateInfo.pNext := nil;
  allocateInfo.commandPool := pool;
  allocateInfo.level := level;
  allocateInfo.commandBufferCount := 1;
  res := vkAllocateCommandBuffers(Fdev, @allocateInfo, @Result);
  if (res <> VK_SUCCESS) then LOG(res, 'Allocating of command buffer failed');
end;

function TDeviceContext.AllocateCommandBuffers(pool: TVkCommandPool;
  level: TVkCommandBufferLevel; commandBufferCount: cardinal): TCommandBuffers;
var allocateInfo: TVkCommandBufferAllocateInfo;
begin
  setlength(Result, commandBufferCount);
  allocateInfo.sType := VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO;
  allocateInfo.pNext := nil;
  allocateInfo.commandPool := pool;
  allocateInfo.level := level;
  allocateInfo.commandBufferCount := commandBufferCount;
  res := vkAllocateCommandBuffers(Fdev, @allocateInfo, @Result[0]);
  if (res <> VK_SUCCESS) then begin
    Result := nil;
    LOG(res, 'Allocating of command buffers failed');
  end;
end;

constructor TDeviceContext.Create(dev: TVkDevice);
begin
  FDev := dev;
  FCommandPools := TCommandPoolList.Create;
end;

function TDeviceContext.CreateCommandPool(queueFamilyIndex: cardinal;
  resetable: boolean; shortTerms: boolean): TVkCommandPool;
var createInfo: TVkCommandPoolCreateInfo;
		flags: TVkCommandPoolCreateFlags;
begin
		flags := 0;
    if resetable then flags := flags + ord(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT);
		if shortTerms then flags := flags + ord(VK_COMMAND_POOL_CREATE_TRANSIENT_BIT);
		createInfo.sType := VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO;
		createInfo.pNext := nil;
		createInfo.flags := flags;
		createInfo.queueFamilyIndex := queueFamilyIndex;
		res := vkCreateCommandPool(FDev, @createInfo, nil, @Result);
    if (res <> VK_SUCCESS) then LOG(res, 'CommandPool creation failed');
end;

destructor TDeviceContext.Destroy;
var i: integer;
begin
  for i:=0 to FCommandPools.Count-1 do begin
     vkDestroyCommandPool(Fdev, FCommandPools[i], nil);
  end;
  FCommandPools.Free;
  vkDeviceWaitIdle(Fdev);
//  vkDestroyDevice(Fdev, nil);
  inherited;
end;

procedure TDeviceContext.ResetCommandBuffer(buffer: TVkCommandBuffer;
  flags: TVkCommandBufferResetFlags);
begin
  res := vkResetCommandBuffer(buffer, flags);
end;

initialization

finalization

end.
