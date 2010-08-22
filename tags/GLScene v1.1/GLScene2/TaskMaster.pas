// TaskMaster
{: Provides Task management and scheduling services.<p>

   <b>History : </b><font size=-1><ul>
      <li>15/11/05 - EG - Creation
   </ul></font>
}
unit TaskMaster;

interface

uses Windows, Classes, SysUtils;

type

   // TTaskInfo
   //
   {: Descibes a task to be scheduled by a TaskMaster. }
   TTaskInfo = class (TObject)
      private
         { Private Declarations }
         FSessionID : Cardinal;
         FExecuting, FCompleted, FAborted : Boolean;
         FOnExecute : TNotifyEvent;
         FSender : TObject;
         FMasterTask : TTaskInfo;
         FLastRunTime : Int64;
         FSubTasks : array of TTaskInfo;
         FSubTaskCount : Integer;
         FSubTasksLock : TRTLCriticalSection;

      protected
         { Protected Declarations }
         //: Notify task info it has been scheduled
         procedure Schedule(masterTask : TTaskInfo);
         //: Notify task info it has been started
         procedure Start;
         //: Notify task info it has stopped
         procedure Stop;
         //: Notify task info is has been aborted
         procedure Abort;

         //: Register a subtask
         procedure RegisterSubTask(subTask : TTaskInfo);
         //: Clear list of all subtasks
         procedure ClearSubTasks;

      public
         { Public Declarations }
         constructor Create(eventOnExecute : TNotifyEvent; aSender : TObject);
         destructor Destroy; override;

         //: Last SessionID upon which this taskInfo was scheduled
         property SessionID : Cardinal read FSessionID;

         //: Event to execute to perform the task
         property OnExecute : TNotifyEvent read FOnExecute write FOnExecute;
         //: Parameter value for the execution event
         property Sender : TObject read FSender write FSender;

         //: Task whose completion is required to execute current task
         property MasterTask : TTaskInfo read FMasterTask;

         {: Returns true if this task has been scheduled for last/current session.<p>
            Returned value is independant from wether the task has been completed
            or not, or wether the session has completed or not. }
         function Scheduled : Boolean;
         {: Returns true if the task is currently executing. }
         function Executing : Boolean;
         {: Returns true if the task has been completed for last/current session.<p>
            If the task hasn't been scheduled for last/current session, returns True. }
         function Completed : Boolean;
         {: Returns true if the task has been aborted for last/current session.<p>
            If the task hasn't been scheduled for last/current session, returns False. }
         function Aborted : Boolean;
   end;

   // TTaskMaster
   //
   {: Abstract TaskMaster class.<p>
      Only one TaskMaster may be instantiated at any time, which is referred
      in the vTaskMaster variable (note: this *may* be relaxed in the future,
      though there will still be only one global TaskMaster) }
   TTaskMaster = class (TObject)
      private
         { Private Declarations }
         FSessionID : Cardinal;
         FSessionCount : Integer;

      protected
         { Protected Declarations }
         procedure ValidateThreadID;

      public
         { Public Declarations }
         constructor Create; virtual;
         destructor Destroy; override;

         //: Initiates a new execution session, during which tasks will be scheduled and executed
         procedure BeginSession; virtual;
         //: End an execution session, waits for completion of all tasks
         procedure EndSession; virtual;
         //: Terminate an execution session (ungracefully)
         procedure AbortSession; virtual;
         //: Is there an active execution session?
         function InSession : Boolean;
         //: Current session ID, valid only if InSession
         property SessionID : Cardinal read FSessionID;

         //: Schedule a task, specifying an optional dependency
         procedure ScheduleTask(aTask : TTaskInfo;
                                dependsOnTask : TTaskInfo = nil); virtual; abstract;

         {: Wait for the task to reach Completed or Aborted status. }
         procedure WaitFor(aTask : TTaskInfo); virtual; abstract;
   end;

   // TSingleThreadedTaskMaster
   //
   {: A TaskMaster for single threaded usage.<p>
      This implementation isn't threaded at all, every task is executed as
      it is specified. }
   TSingleThreadedTaskMaster = class (TTaskMaster)
      private
         { Private Declarations }

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         procedure ScheduleTask(aTask : TTaskInfo; dependsOnTask : TTaskInfo = nil); override;
         procedure WaitFor(aTask : TTaskInfo); override;
   end;

   ETaskMasterException = class(Exception);

function GlobalTaskMaster : TTaskMaster;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vTaskMaster : TTaskMaster;
   vValidSchedulingThreadID : Cardinal;

// GlobalTaskMaster
//
function GlobalTaskMaster : TTaskMaster;
begin
   Result:=vTaskMaster;
end;

// RDTSC
//
function RDTSC : Int64;
asm
   db $0f, $31
end;

// RaiseTaskMasterException
//
procedure RaiseTaskMasterException(const msg : String);
begin
   raise ETaskMasterException.Create(msg);
end;

// ------------------
// ------------------ TTaskInfo ------------------
// ------------------

// Create
//
constructor TTaskInfo.Create(eventOnExecute : TNotifyEvent; aSender : TObject);
begin
   FOnExecute:=eventOnExecute;
   FSender:=aSender;
   InitializeCriticalSection(FSubTasksLock);
end;

// Destroy
//
destructor TTaskInfo.Destroy;
begin
   if vTaskMaster.InSession then
      RaiseTaskMasterException('Destroying a Task during an Execution is not allowed');
   DeleteCriticalSection(FSubTasksLock);
end;

// Scheduled
//
function TTaskInfo.Scheduled : Boolean;
begin
   Result:=(FSessionID=vTaskMaster.SessionID);
end;

// Executing
//
function TTaskInfo.Executing : Boolean;
begin
   Result:=FExecuting;
end;

// Completed
//
function TTaskInfo.Completed : Boolean;
begin
   Result:=FCompleted or (FSessionID<vTaskMaster.SessionID);
end;

// Aborted
//
function TTaskInfo.Aborted : Boolean;
begin
   Result:=FAborted and (FSessionID<vTaskMaster.SessionID);
end;

// Schedule
//
procedure TTaskInfo.Schedule(masterTask : TTaskInfo);
begin
   FSessionID:=vTaskMaster.SessionID;
   FMasterTask:=masterTask;
   if masterTask<>nil then
      masterTask.RegisterSubTask(Self);
   FAborted:=False;
   FCompleted:=False;
   FExecuting:=False;
   ClearSubTasks;
end;

// Start
//
procedure TTaskInfo.Start;
begin
   FExecuting:=True;
   FLastRunTime:=RDTSC;
end;

// Stop
//
procedure TTaskInfo.Stop;
begin
   FExecuting:=False;
   FCompleted:=True;
   FLastRunTime:=RDTSC-FLastRunTime;
end;

// Abort
//
procedure TTaskInfo.Abort;
begin
   FExecuting:=False;
   FAborted:=True;
   FLastRunTime:=0;
end;

// RegisterSubTask
//
procedure TTaskInfo.RegisterSubTask(subTask : TTaskInfo);
var
   n : Integer;
begin
   EnterCriticalSection(FSubTasksLock);

   n:=Length(FSubTasks);
   if FSubTaskCount<=n then
      SetLength(FSubTasks, 2*n+1);
   FSubTasks[FSubTaskCount]:=subTask;
   Inc(FSubTaskCount);

   LeaveCriticalSection(FSubTasksLock);
end;

// ClearSubTasks
//
procedure TTaskInfo.ClearSubTasks;
begin
   // shouldn't require a lock here, it's invoked from main thread only via Schedule
   // and scheduling hasn't been completed yet, so no new subtask can be attached
   FSubTaskCount:=0;
end;

// ------------------
// ------------------ TTaskMaster ------------------
// ------------------

// Create
//
constructor TTaskMaster.Create;
begin
   ValidateThreadID;
   if vTaskMaster<>nil then
      RaiseTaskMasterException('Only ONE TaskMaster may be instantiated at any time');
   vTaskMaster:=Self;
end;

// Destroy
//
destructor TTaskMaster.Destroy;
begin
   if vTaskMaster<>Self then
      RaiseTaskMasterException('TaskMaster instancing anomaly');
   if InSession then
      AbortSession;
   vTaskMaster:=nil;
end;

// BeginSession
//
procedure TTaskMaster.BeginSession;
begin
   ValidateThreadID;
   if FSessionCount>=1 then
      RaiseTaskMasterException('Nested sessions not supported');
   Inc(FSessionCount);
   Inc(FSessionID);
end;

// EndSession
//
procedure TTaskMaster.EndSession;
begin
   ValidateThreadID;
   if FSessionCount<=0 then
      RaiseTaskMasterException('Unbalanced EndSession');
   Dec(FSessionCount);
end;

// AbortSession
//
procedure TTaskMaster.AbortSession;
begin
   ValidateThreadID;
   if FSessionCount<=0 then
      RaiseTaskMasterException('Unbalanced AbortSession');
   Dec(FSessionCount);
end;

// InSession
//
function TTaskMaster.InSession : Boolean;
begin
   Result:=(FSessionCount>0);
end;

// ValidateThreadID
//
procedure TTaskMaster.ValidateThreadID;
begin
   Assert(vValidSchedulingThreadID=GetCurrentThreadID,
          'TaskMaster can only be controled from the main thread!');         
end;

// ------------------
// ------------------ TSingleThreadedTaskMaster ------------------
// ------------------

// ScheduleTask
//
procedure TSingleThreadedTaskMaster.ScheduleTask(aTask : TTaskInfo; dependsOnTask : TTaskInfo = nil);
begin
   ValidateThreadID;
   if not InSession then
      RaiseTaskMasterException('Can only schedule task in an execution session!');
   if dependsOnTask<>nil then
      if dependsOnTask.SessionID<>SessionID then
         RaiseTaskMasterException('Depended-on task must be schedule first!');
   if aTask.SessionID>=SessionID then
      RaiseTaskMasterException('Task has already been scheduled in this session!');
   aTask.Schedule(dependsOnTask);
   aTask.Start;
   try
      aTask.OnExecute(aTask.Sender);
      aTask.Stop;
   except
      aTask.Abort;
   end;
end;

// WaitFor
//
procedure TSingleThreadedTaskMaster.WaitFor(aTask : TTaskInfo);
begin
   ValidateThreadID;
   if not aTask.Completed then begin
      if aTask.Aborted then
         RaiseTaskMasterException('Task has been aborted')
      else RaiseTaskMasterException(ClassName+' has a big bug!');
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vValidSchedulingThreadID:=GetCurrentThreadID;
   TSingleThreadedTaskMaster.Create;

finalization

   vTaskMaster.Free;

end.
