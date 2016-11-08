//
// VKScene Component Library, based on GLScene http://glscene.sourceforge.net 
//
{
   Implements a HDS Filter that generates HeightData tiles in a seperate thread. 

   This component is a TVKHeightDataSourceFilter, which uses a TVKHeightDataSourceThread,
   to asyncronously search the HeightData cache for any queued tiles.
   When found, it then prepares the queued tile in its own TVKHeightDataThread.

   This allows the GUI to remain responsive, and prevents freezes when new tiles are
   being prepared.  Although this keeps the framerate up, it may cause holes in the
   terrain to show, if the HeightDataThreads cant keep up with the TerrainRenderer's
   requests for new tiles.
   The history of development is listed in a former version of GLScene.  
     
}

unit VKS.AsyncHDS;

interface

{$I VKScene.inc}

uses
  System.Classes, System.SysUtils,
  VKS.HeightData, VKS.CrossPlatform;

type
  TVKAsyncHDS = class;
  TIdleEvent = procedure(Sender:TVKAsyncHDS;TilesUpdated:boolean) of object;
  TNewTilePreparedEvent = procedure (Sender : TVKAsyncHDS; heightData : TVKHeightData) of object; //a tile was updated (called INSIDE the sub-thread?)

  // TUseDirtyTiles
  //
  {  TUseDirtyTiles determines if/how dirty tiles are displayed and when they are released.
      
      TUseDirtyTiles
      
       When a tile is maked as dirty, a replacement is queued immediately.
       However, the replacement cant be used until the HDThread has finished preparing it.
       Dirty tiles can be deleted as soon as they are no longer used/displayed.

     Possible states for a TUseDirtyTiles. 
       hdsNever :            Dirty tiles get released immediately, leaving a hole in the terrain, until the replacement is hdsReady.
       hdsUntilReplaced :    Dirty tiles are used, until the HDThread has finished preparing the queued replacement.
       hdsUntilAllReplaced : Waits until the HDSThread has finished preparing ALL queued tiles,
                             before allowing the renderer to switch over to the new set of tiles.
                             (This prevents a fading checkerbox effect.)
        }
  TUseDirtyTiles=(dtNever,dtUntilReplaced,dtUntilAllReplaced);


	 TVKAsyncHDS = class (TVKHeightDataSourceFilter)
	   private
	      { Private Declarations }
       FOnIdleEvent :TIdleEvent;
       FOnNewTilePrepared : TNewTilePreparedEvent;
       FUseDirtyTiles:TUseDirtyTiles;
       FTilesUpdated:boolean;
	   protected
	      { Protected Declarations }
    public
      //TilesUpdated:boolean;
	      { Public Declarations }
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure BeforePreparingData(heightData : TVKHeightData); override;
      procedure StartPreparingData(heightData : TVKHeightData); override;
      procedure ThreadIsIdle; override;
      procedure NewTilePrepared(heightData:TVKHeightData);
      function  ThreadCount:integer;
      procedure WaitFor(TimeOut:integer=2000);
      //procedure NotifyChange(Sender : TObject); override;
      function  TilesUpdated:boolean;        //Returns true if tiles have been updated since the flag was last reset
      procedure TilesUpdatedFlagReset;       //sets the TilesUpdatedFlag to false; (is ThreadSafe)
	   published
	      { Published Declarations }
      property OnIdle : TIdleEvent read FOnIdleEvent write FOnIdleEvent;
      property OnNewTilePrepared : TNewTilePreparedEvent read FOnNewTilePrepared write FOnNewTilePrepared;
      property UseDirtyTiles :TUseDirtyTiles read FUseDirtyTiles write FUseDirtyTiles;
      property MaxThreads;         //sets the maximum number of simultaineous threads that will prepare tiles.(>1 is rarely needed)
      property Active;             //set to false, to ignore new queued tiles.(Partially processed tiles will still be completed)
  end;

  TVKAsyncHDThread = class(TVKHeightDataThread)
    public
      Owner : TVKAsyncHDS;
      HDS   : TVKHeightDataSource;
      Procedure Execute; override;
      Procedure Sync;
  end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TVKAsyncHDS ------------------
// ------------------

// Create
//
constructor TVKAsyncHDS.Create(AOwner: TComponent);
begin
	 inherited Create(AOwner);
  MaxThreads:=1;
  FUseDirtyTiles:=dtNever;
  FTilesUpdated:=true;
end;

// Destroy
//
destructor TVKAsyncHDS.Destroy;
begin
	inherited Destroy;
end;

// BeforePreparingData
//
procedure TVKAsyncHDS.BeforePreparingData(heightData : TVKHeightData);
begin
  if FUseDirtyTiles=dtNever then begin
    if heightData.OldVersion<>nil then begin
      heightData.OldVersion.DontUse:=true;
      heightData.DontUse:=false;
    end;
  end;
  if assigned(HeightDataSource) then HeightDataSource.BeforePreparingData(heightData);
end;

// StartPreparingData
//
procedure TVKAsyncHDS.StartPreparingData(heightData : TVKHeightData);
var HDThread : TVKAsyncHDThread;
    HDS:TVKHeightDataSource;
begin
  HDS:=HeightDataSource;
  //---if there is no linked HDS then return an empty tile--
  if not Assigned(HDS) then begin
    heightData.DataState:=hdsNone;
    exit;
  end;
  if (Active=false) then exit;

  //---If not using threads then prepare the HD tile directly---  (everything else freezes until done)
  if MaxThreads=0 then begin
    HDS.StartPreparingData(HeightData);
    if heightData.DataState=hdsPreparing
      then heightData.DataState:=hdsReady
      else heightData.DataState:=hdsNone;
  end else begin //--MaxThreads>0 : start the thread and go back to start the next one--
    heightData.DataState:=hdsPreparing; //prevent other threads from preparing this HD.
    HDThread:=TVKAsyncHDThread.Create(true);
    HDThread.Owner:=self;
    HDThread.HDS:=self.HeightDataSource;
    HDThread.HeightData:=HeightData;
    heightData.Thread:=HDThread;
    HDThread.FreeOnTerminate:=false;
    HDThread.Start;
  end;
end;


//OnIdle event
//
procedure TVKAsyncHDS.ThreadIsIdle;
var i:integer;
    lst:TList;
    HD:TVKHeightData;
begin
  //----------- dtUntilAllReplaced -------------
  //Switch to the new version of ALL dirty tiles
    lst:=self.Data.LockList;
    try
      if FUseDirtyTiles=dtUntilAllReplaced then begin
        i:=lst.Count;
        while(i>0) do begin
          dec(i);
          HD:=TVKHeightData(lst.Items[i]);
          if (HD.DataState in [hdsReady,hdsNone])
            and(Hd.DontUse)and(HD.OldVersion<>nil) then begin
            HD.DontUse:=false;
            HD.OldVersion.DontUse:=true;
            FTilesUpdated:=true;
          end;
        end;
      end;//Until All Replaced
      if Assigned(FOnIdleEvent) then FOnIdleEvent(Self,FTilesUpdated);
    finally
      self.Data.UnlockList;
    end;
  //--------------------------------------------
end;

//OnNewTilePrepared event
//
procedure TVKAsyncHDS.NewTilePrepared(heightData:TVKHeightData);
var HD:TVKHeightData;
begin
  if assigned(HeightDataSource) then HeightDataSource.AfterPreparingData(HeightData);
  with self.Data.LockList do begin
    try
      HD:=heightdata;
      //--------------- dtUntilReplaced -------------
      //Tell terrain renderer to display the new tile
      if (FUseDirtyTiles=dtUntilReplaced)and(HD.DontUse)and(HD.OldVersion<>nil) then begin
        HD.DontUse:=false;            //No longer ignore the new tile
        HD.OldVersion.DontUse:=true;  //Start ignoring the old tile
      end;
      //---------------------------------------------
      if HD.DontUse=false then FTilesUpdated:=true;
      if Assigned(FOnNewTilePrepared) then FOnNewTilePrepared(Self,HeightData);           //OnNewTilePrepared Event
    finally
      self.Data.UnlockList;
    end;
  end;
end;

//ThreadCount
//  Count the active threads
//
function TVKAsyncHDS.ThreadCount:integer;
var lst: Tlist;
    i,TdCtr:integer;
    HD:TVKHeightData;
begin
  lst:=self.Data.LockList;
  i:=0;TdCtr:=0;
  while(i<lst.Count)and(TdCtr<self.MaxThreads) do begin
    HD:=TVKHeightData(lst.Items[i]);
    if HD.Thread<>nil then Inc(TdCtr);
    inc(i);
  end;
  self.Data.UnlockList;
  result:=TdCtr;
end;

//WaitFor
//  Wait for all running threads to finish.
//  Should only be called after setting Active to false,
//  to prevent new threads from starting.
procedure TVKAsyncHDS.WaitFor(TimeOut:Integer=2000);
var OutTime:TDateTime;
begin
  Assert(self.active=false);
  OutTime:=now+TimeOut;
  While ((now<OutTime)and(ThreadCount>0)) do begin
    sleep(0);
  end;
  Assert(ThreadCount=0);
end;

{
procedure TVKAsyncHDS.NotifyChange(Sender : TObject);
begin
  TilesChanged:=true;
end;
}

// This function prevents the user from trying to write directly to this variable.
// FTilesUpdated if NOT threadsafe and should only be reset with TilesUpdatedFlagReset.
function TVKAsyncHDS.TilesUpdated:boolean;
begin
  result:=FTilesUpdated;
end;

// Set the TilesUpdatedFlag to false. (is Threadsafe)
procedure TVKAsyncHDS.TilesUpdatedFlagReset;
begin
  if not assigned(self) then exit; //prevents AV on Application termination.
  with Data.LockList do try
    FTilesUpdated:=False;
  finally Data.UnlockList; end;
end;

//-------------------HD Thread----------------
Procedure TVKAsyncHDThread.Execute;
Begin
  HDS.StartPreparingData(HeightData);
  HeightData.Thread:=nil;
  Synchronize(sync);
end;

Procedure TVKAsyncHDThread.Sync;
begin
  Owner.NewTilePrepared(heightData);
  if heightData.DataState=hdsPreparing then heightData.DataState:=hdsReady;
end;

//--------------------------------------------

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClass(TVKAsyncHDS);

end.
