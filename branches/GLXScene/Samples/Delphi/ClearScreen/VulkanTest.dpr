{$APPTYPE CONSOLE}

{$define VK_USE_PLATFORM_WIN32_KHR}
{.$define DEBUG_LAYERS ON}

uses winapi.Windows, Vulkan, vulkan_app;

var
  wnd: TWin32Window = nil;
  use_device: integer = -1;

function WindowProc(hWnd,Msg:Longint; wParam : WPARAM; lParam: LPARAM):Longint; stdcall;
begin
	wnd.handleMessages(hWnd, Msg, wParam, lParam);
  Result:=DefWindowProc(hWnd,Msg,wParam,lParam);
end;

Type

  TAppInfo = record
    dev: TVkDevice;
    dc: TDeviceContext;
    pool: TVkCommandPool;
    cmd: TCommandBuffers;
    GraphicsQueueIndex: cardinal;
    PresenterQueueIndex: cardinal;
    ImageAvailableSemaphore: TVkSemaphore;
    RenderingFinishedSemaphore: TVkSemaphore;
    swapchain: TSwapchain;
  end;


  TVulkanApp = class
  private
    Finst: TVulkanInstance;
    FWnd: TWin32Window;
    FAppInfo: TAppInfo;
    FcurrentBuffer: cardinal;
    FcbInfo: TVkCommandBufferBeginInfo;
    FClearColor: TVkClearColorValue;
    Fwait_dst_stage_mask: TVkPipelineStageFlags;
    Fsubmit_info: TVkSubmitInfo;
    Fpresent_info: TVkPresentInfoKHR;
    Fqueue: TVkQueue;
    FInitialized: boolean;
    FSemaphore_create_info: TVkSemaphoreCreateInfo;
    procedure PerformClear;
    procedure Clear(cmd: TVkCommandBuffer; image: TVkImage; clearColor: TVkClearColorValue);
  public
    constructor Create(wnd: TWin32Window);
    destructor Destroy;override;

    property AppInfo: TAppInfo read FAppInfo;
    property Inst: TVulkanInstance read FInst;
    property ClearColor: TVkClearColorValue read FClearColor write FClearColor;

    procedure Initialize(device_index: integer = 0);

    procedure KeyPressed(keyCode: cardinal);
    procedure Render;
    procedure RenderLoop;
  end;

constructor TVulkanApp.Create(wnd: TWin32Window);
var
  ext: TExtentionsList;
  layers: TLayersList;
begin
  FWnd := wnd;
  FInitialized := false;

  ext :=TExtentionsList.Create;
  ext.Add(VK_KHR_SURFACE_EXTENSION_NAME);
  ext.Add(VK_KHR_WIN32_SURFACE_EXTENSION_NAME);

  {$ifdef DEBUG_LAYERS}
  layers := TLayersList.Create;
	layers.Add(PAnsiChar('VK_LAYER_LUNARG_swapchain'));
  layers.Add(PAnsiChar('VK_LAYER_LUNARG_core_validation'));
	layers.Add(PAnsiChar('VK_LAYER_LUNARG_standard_validation'));
  {$else}
  layers := nil;
  {$endif}
  Finst := TVulkanInstance.Create(wnd, ext, layers);
  ext.Free; layers.Free;

  Fsemaphore_create_info.sType := VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO;
  Fsemaphore_create_info.pNext := nil;
  Fsemaphore_create_info.flags := 0;

  FcbInfo.sType := VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO;
  FcbInfo.pNext := nil;
  FcbInfo.flags := 0;
  FcbInfo.pInheritanceInfo := nil;

  with FClearColor do begin
    float32[0] := 0.0; float32[1] := 1.0; float32[2] := 0.0; float32[3] := 1.0;
  end;
end;

destructor TVulkanApp.Destroy;
begin
  if FInitialized then begin
    vkDestroySemaphore(FAppInfo.dev, FAppInfo.ImageAvailableSemaphore, nil);
    vkDestroySemaphore(FAppInfo.dev, FAppInfo.RenderingFinishedSemaphore, nil);

    if assigned(FAppInfo.dc) then FAppInfo.dc.Free;
    if FAppInfo.dev <> VK_NULL_HANDLE then begin
      vkDeviceWaitIdle(FAppInfo.dev);
      if assigned(FAppInfo.swapchain) then FAppInfo.swapchain.Free;
      vkDestroyDevice(FAppInfo.dev, nil);
    end;
  end;

  Finst.Free;
  inherited;
end;

procedure TVulkanApp.Initialize(device_index: integer);
begin

  FAppInfo.dev := Finst.Devices[device_index].CreatePresenter;
  FAppInfo.GraphicsQueueIndex := Finst.Devices[device_index].GraphicsQueueIndex;
  FAppInfo.PresenterQueueIndex := Finst.Devices[device_index].PresenterQueueIndex;
  FAppInfo.swapchain := TSwapChain.Create(FAppInfo.dev, Finst.Devices[device_index]);
  FAppInfo.swapchain.Initialize;
  FAppInfo.dc := TDeviceContext.Create(FAppInfo.dev);
  FAppInfo.pool := FAppInfo.dc.CreateCommandPool(FAppInfo.GraphicsQueueIndex, false, false);
  FAppInfo.cmd := FAppInfo.dc.AllocateCommandBuffers(FAppInfo.pool,
    VK_COMMAND_BUFFER_LEVEL_PRIMARY, FAppInfo.swapchain.Images.Count);

  PerformClear;

  vkCreateSemaphore(FAppInfo.dev, @Fsemaphore_create_info, nil, @FAppInfo.ImageAvailableSemaphore);
  vkCreateSemaphore(FAppInfo.dev, @Fsemaphore_create_info, nil, @FAppInfo.RenderingFinishedSemaphore);

  vkGetDeviceQueue(FAppInfo.dev, FAppInfo.GraphicsQueueIndex, 0, @Fqueue);

  Fwait_dst_stage_mask := ord(VK_PIPELINE_STAGE_TRANSFER_BIT);
  Fsubmit_info.sType := VK_STRUCTURE_TYPE_SUBMIT_INFO;
  Fsubmit_info.pNext := nil;
  Fsubmit_info.waitSemaphoreCount := 1;
  Fsubmit_info.pWaitSemaphores := @FAppInfo.ImageAvailableSemaphore;
  Fsubmit_info.pWaitDstStageMask := @Fwait_dst_stage_mask;
  Fsubmit_info.commandBufferCount := 1;
  Fsubmit_info.signalSemaphoreCount := 1;
  Fsubmit_info.pSignalSemaphores := @FAppInfo.RenderingFinishedSemaphore;

  Fpresent_info.sType := VK_STRUCTURE_TYPE_PRESENT_INFO_KHR;
  Fpresent_info.pNext := nil;
  Fpresent_info.waitSemaphoreCount := 1;
  Fpresent_info.pWaitSemaphores := @FAppInfo.RenderingFinishedSemaphore;
  Fpresent_info.swapchainCount := 1;
  Fpresent_info.pSwapchains := @FAppInfo.swapchain.Swapchain;
  Fpresent_info.pImageIndices := @FcurrentBuffer;
  Fpresent_info.pResults := nil;

  FInitialized := true;

end;

procedure TVulkanApp.KeyPressed(keyCode: cardinal);
begin
   //do nothing
end;

procedure TVulkanApp.PerformClear;
var i: integer;
begin
  for i:=0 to FAppInfo.swapchain.Images.Count-1 do begin
    vkBeginCommandBuffer(FAppInfo.cmd[i], @FcbInfo);
    Clear(FAppInfo.cmd[i], FAppInfo.swapchain.Images[i].image, ClearColor);
    vkEndCommandBuffer(FAppInfo.cmd[i]);
  end;
end;

procedure TVulkanApp.Render;
var cmd: TVkCommandBuffer;
begin
		vkAcquireNextImageKHR(FAppInfo.dev, FAppinfo.swapchain.Swapchain, High(Int64),
      FAppInfo.ImageAvailableSemaphore, TVkFence(nil), @FcurrentBuffer);
		cmd := FAppinfo.cmd[FcurrentBuffer];
		Fsubmit_info.pCommandBuffers := @cmd;
		vkQueueSubmit(Fqueue, 1, @Fsubmit_info, VK_NULL_HANDLE);
		vkQueuePresentKHR(Fqueue, @Fpresent_info);
		vkDeviceWaitIdle(FAppInfo.dev);
end;

procedure TVulkanApp.RenderLoop;
var msg: TMSG;
begin
  while GetMessage(Msg, 0, 0, 0) <> BOOL(FALSE) do begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
 		render();
  end;
end;

procedure TVulkanApp.Clear(cmd: TVkCommandBuffer; image: TVkImage; clearColor: TVkClearColorValue);
var
  barrier_from_present_to_clear: TVkImageMemoryBarrier;
  barrier_from_clear_to_present: TVkImageMemoryBarrier;
  image_subresource_range: TVkImageSubresourceRange;
begin
  image_subresource_range.aspectMask := ord(VK_IMAGE_ASPECT_COLOR_BIT);
  image_subresource_range.baseMipLevel := 0;
  image_subresource_range.levelCount := 1;
  image_subresource_range.baseArrayLayer := 0;
  image_subresource_range.layerCount := 1;

  barrier_from_present_to_clear.image := image;
  barrier_from_clear_to_present.image := image;

	with barrier_from_present_to_clear do begin
    sType := VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
    pNext := nil;
    srcAccessMask := 0;
    dstAccessMask := ord(VK_ACCESS_TRANSFER_WRITE_BIT);
    oldLayout := VK_IMAGE_LAYOUT_UNDEFINED;
    newLayout := VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
    srcQueueFamilyIndex := VK_QUEUE_FAMILY_IGNORED;
    dstQueueFamilyIndex := VK_QUEUE_FAMILY_IGNORED;
    subresourceRange := image_subresource_range;
	end;


	with barrier_from_clear_to_present do begin
    sType := VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER;
    pNext := nil;
    srcAccessMask := ord(VK_ACCESS_TRANSFER_WRITE_BIT);
    dstAccessMask := ord(VK_ACCESS_MEMORY_READ_BIT);
    oldLayout := VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL;
    newLayout := VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
    srcQueueFamilyIndex := VK_QUEUE_FAMILY_IGNORED;
    dstQueueFamilyIndex := VK_QUEUE_FAMILY_IGNORED;
    subresourceRange := image_subresource_range;
	end;

	vkCmdPipelineBarrier(cmd, ord(VK_PIPELINE_STAGE_TRANSFER_BIT), ord(VK_PIPELINE_STAGE_TRANSFER_BIT), 0, 0, nil, 0, nil, 1, @barrier_from_present_to_clear);
	vkCmdClearColorImage(cmd, image, VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, @clearColor, 1, @image_subresource_range);
	vkCmdPipelineBarrier(cmd, ord(VK_PIPELINE_STAGE_TRANSFER_BIT), ord(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT), 0, 0, nil, 0, nil, 1, @barrier_from_clear_to_present);
end;


var
  app: TVulkanApp;
  i: integer;
  s: string;

begin
  //Create Window for Vulkan
  wnd := TWin32Window.Create(800, 600, 'Vulkan Example', 'VulkanExample',@WindowProc);

  //Load library and initialize Vulkan
  if LoadVulkanLibrary then
    writeln('LoadVulkanLibrary was successfully . . .');
  if LoadVulkanGlobalCommands then
    writeln('LoadVulkanGlobalCommands was successfully . . .');

  if (@vkCreateInstance = nil) or (@vkCreateSwapchainKHR = nil) then begin
    writeln('[Error] Loading Vulkan library failed');
    Exit;
  end;

  app := TVulkanApp.Create(wnd);

  //Print some of Vulkan properties
  with app do begin
    for i := 0 to inst.Layers.Count-1 do writeln('Layer: ', copy(inst.Layers[i].layerName, 0, VK_MAX_EXTENSION_NAME_SIZE));
    for i := 0 to inst.Extentions.Count-1 do writeln('Ext: ', copy(inst.Extentions[i].extensionName, 0, VK_MAX_EXTENSION_NAME_SIZE));
    for i := 0 to inst.Devices.Count-1 do begin
      writeln('GPU ',i,': ', copy(inst.Devices[i].info.deviceName, 0, VK_MAX_PHYSICAL_DEVICE_NAME_SIZE));
      writeln(#9+'Support presenting: ', inst.Devices[i].isDeviceSupportPresenting);
      writeln(#9+'Surface Capabilities:');
      with inst.Devices[i].Surface.capabilities do begin
        writeln(#9+' minImageCount: ', minImageCount);
        writeln(#9+' maxImageCount: ', maxImageCount);
        writeln(#9+' currentExtent  (w,h): ', '[', currentExtent.width, ',' , currentExtent.height, ']');
        writeln(#9+' minImageExtent (w,h): ', '[', minImageExtent.width, ',' , minImageExtent.height, ']');
        writeln(#9+' maxImageExtent (w,h): ', '[', maxImageExtent.width, ',' , maxImageExtent.height, ']');
        writeln(#9+' maxImageArrayLayers: ', maxImageArrayLayers);
        writeln(#9+' supportedTransforms: ', supportedTransforms);
        writeln(#9+' currentTransform: ', cardinal(currentTransform));
        writeln(#9+' supportedCompositeAlpha: ', supportedCompositeAlpha);
        writeln(#9+' supportedUsageFlags: ', supportedUsageFlags);
      end;
    end;
  end;

  if app.inst.Devices.Count > 1 then begin
    writeln('Found several Vulkan capatible deviceses.');
    writeln('Please specify device index');
    ShowWindow(wnd.Window, SW_HIDE);
    readln(use_device);

    if (use_device < 0) or (use_device>=app.inst.Devices.Count) then begin
      writeln('Incorrect device index(',use_device,')');
      app.Free; wnd.Free; halt(0);
    end;
    ShowWindow(wnd.Window, SW_SHOW);
  end else use_device := 0;

  app.Initialize(use_device);

  app.RenderLoop;

  app.Free; wnd.Free;

end.
