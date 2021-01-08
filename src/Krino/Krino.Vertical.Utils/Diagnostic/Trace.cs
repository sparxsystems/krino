using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Threading;

namespace Krino.Vertical.Utils.Diagnostic
{
    /// <summary>
    /// Super duper trace.
    /// </summary>
    public class Trace : ILogger, IDisposable
    {
        private int myThreadId;
        private long myEnteringTicks;
        private StackFrame myCallStack;
        private ProfilerData myBufferedProfileData;
        private bool myIsTiming;
        private string myTimingRegionName;

        private static ulong myProfilerCounter;

        private static ManualResetEvent myQueueThreadEndedEvent = new ManualResetEvent(true);
        private static bool myProcessingIsRunning;
        private static Queue<Action> myTraceQueue = new Queue<Action>(1000000);

        private class ProfilerData
        {
            public long Calls;
            public long Ticks;
            public long SubcallsTicks;
            public int MaxConcurency;
            public int MaxRecursion;

            public Dictionary<int, int> Threads = new Dictionary<int, int>();
        }

        private static long myStartProfilingTicks;
        private static Dictionary<MethodBase, ProfilerData> myProfilerData = new Dictionary<MethodBase, ProfilerData>();
        private static Dictionary<int, Stack<ProfilerData>> myActiveStacks = new Dictionary<int, Stack<ProfilerData>>();
        private static volatile bool myProfilerIsRunning;

        [DllImport("Kernel32.dll", CallingConvention = CallingConvention.Winapi)]
        static extern void GetSystemTimePreciseAsFileTime(out long filetime);


        /// <summary>
        /// Private helper constructor.
        /// </summary>
        /// <remarks>
        /// The constructor is private, so the class can be enstantiating only via the 'Entering' method.
        /// </remarks>
        private Trace()
        {
        }


        /// <summary>
        /// The GetSystemTimePreciseAsFileTime API provides more precise timing information.
        /// It works only on Windows 8+ platforms.
        /// </summary>
        public static bool UseGetSystemTimePreciseAsFileTime { get; set; } = true;

        /// <summary>
        /// Traces entering-leaving the method.
        /// </summary>
        /// <param name="additionalInfo">additional info which will be displayed for the enetered method</param>
        /// <remarks>
        /// The enetering information for the method calling this constructor is put to the trace
        /// and the measuring of the time starts.
        /// In order to trace entering-leaving, the detail level must be set to 'Debug'.
        /// </remarks>
        public static IDisposable Entering(string additionalInfo = null)
        {
            Trace aTraceObject = null;

            if (DetailLevel <= LogLevel.Debug || myProfilerIsRunning)
            {
                aTraceObject = new Trace();
                aTraceObject.myCallStack = new StackFrame(1);
                aTraceObject.myEnteringTicks = GetTimeTicks();

                if (myProfilerIsRunning)
                {
                    UpdateProfilerForEntering(aTraceObject);
                }
                else
                {
                    WriteMessage(aTraceObject.myCallStack, aTraceObject.myEnteringTicks, TraceEntryType.EnteringMethod, additionalInfo);
                }
            }

            return aTraceObject;
        }

        /// <summary>
        /// Traces the leaving from the method including the duration time.
        /// </summary>
        void IDisposable.Dispose()
        {
            try
            {
                if (myEnteringTicks != 0)
                {
                    long aLeavingTicks = GetTimeTicks();
                    long aElapsedTicks = aLeavingTicks - myEnteringTicks;

                    if (myProfilerIsRunning)
                    {
                        UpdateProfilerForLeaving(this, aElapsedTicks);
                    }
                    else if (DetailLevel < LogLevel.None && myIsTiming)
                    {
                        WriteMessage(myCallStack, aLeavingTicks, TraceEntryType.Timing, myTimingRegionName, null, aElapsedTicks);
                    }
                    else if (DetailLevel <= LogLevel.Debug)
                    {
                        WriteMessage(myCallStack, aLeavingTicks, TraceEntryType.LeavingMethod, null, null, aElapsedTicks);
                    }
                }
            }
            catch
            {
                // Any exception in this Dispose method is irrelevant.
            }
        }

        /// <summary>
        /// Traces elapsed time for a secified code section.
        /// </summary>
        /// <remarks>
        /// It starts to measure the time for the section of the code.
        /// </remarks>
        /// <param name="timingRegionName">name of the section in the code which shall be measured</param>
        /// <returns></returns>
        public static IDisposable EnterTiming(string timingRegionName = null)
        {
            Trace aTraceObject = null;

            if (DetailLevel < LogLevel.None && !myProfilerIsRunning)
            {
                aTraceObject = new Trace();
                aTraceObject.myIsTiming = true;
                aTraceObject.myTimingRegionName = timingRegionName;
                aTraceObject.myCallStack = new StackFrame(1);
                aTraceObject.myEnteringTicks = GetTimeTicks();
            }

            return aTraceObject;
        }


        public bool IsEnabled(LogLevel logLevel) => DetailLevel == logLevel;

        public IDisposable BeginScope<TState>(TState state) => default;
        


        /// <summary>
        /// Traces the info message.
        /// </summary>
        /// <param name="message">info message</param>
        public static void Info(string message)
        {
            if (DetailLevel <= LogLevel.Information)
            {
                StackFrame aCallStack = new StackFrame(1);

                long aTimeTicks = GetTimeTicks();
                WriteMessage(aCallStack, aTimeTicks, TraceEntryType.Info, message, null);
            }
        }

        /// <summary>
        /// Traces the info message.
        /// </summary>
        /// <param name="message">info message</param>
        /// <param name="err">exception that will be traced</param>
        public static void Info(string message, Exception err)
        {
            if (DetailLevel <= LogLevel.Information)
            {
                StackFrame aCallStack = new StackFrame(1);

                long aTimeTicks = GetTimeTicks();
                WriteMessage(aCallStack, aTimeTicks, TraceEntryType.Info, message, err);
            }
        }

        /// <summary>
        /// Traces warning message.
        /// </summary>
        /// <param name="message">warning message</param>
        public static void Warning(string message)
        {
            if (DetailLevel <= LogLevel.Warning)
            {
                StackFrame aCallStack = new StackFrame(1);

                long aTimeTicks = GetTimeTicks();
                WriteMessage(aCallStack, aTimeTicks, TraceEntryType.Warning, message, null);
            }
        }

        /// <summary>
        /// Traces the warning message.
        /// </summary>
        /// <param name="message">warning message</param>
        /// <param name="err">exception that will be traced</param>
        public static void Warning(string message, Exception err)
        {
            if (DetailLevel <= LogLevel.Warning)
            {
                StackFrame aCallStack = new StackFrame(1);

                long aTimeTicks = GetTimeTicks();
                WriteMessage(aCallStack, aTimeTicks, TraceEntryType.Warning, message, err);
            }
        }

        /// <summary>
        /// Traces the error message.
        /// </summary>
        /// <param name="message">error message</param>
        public static void Error(string message)
        {
            if (DetailLevel <= LogLevel.Critical)
            {
                StackFrame aCallStack = new StackFrame(1);

                long aTimeTicks = GetTimeTicks();
                WriteMessage(aCallStack, aTimeTicks, TraceEntryType.Error, message, null);
            }
        }

        /// <summary>
        /// Traces the error message.
        /// </summary>
        /// <param name="message">error message</param>
        /// <param name="err">exception that will be traced</param>
        public static void Error(string message, Exception err)
        {
            if (DetailLevel <= LogLevel.Critical)
            {
                StackFrame aCallStack = new StackFrame(1);

                long aTimeTicks = GetTimeTicks();
                WriteMessage(aCallStack, aTimeTicks, TraceEntryType.Error, message, err);
            }
        }

        /// <summary>
        /// Traces the debug message.
        /// </summary>
        /// <remarks>
        /// To trace debug messages, the detail level must be set to debug.
        /// </remarks>
        /// <param name="message">error message</param>
        public static void Debug(string message)
        {
            if (DetailLevel <= LogLevel.Debug)
            {
                StackFrame aCallStack = new StackFrame(1);

                long aTimeTicks = GetTimeTicks();
                WriteMessage(aCallStack, aTimeTicks, TraceEntryType.Debug, message, null);
            }
        }

        /// <summary>
        /// Starts the profiler measurement.
        /// </summary>
        public static void StartProfiler()
        {
            lock (myProfilerData)
            {
                Logger?.LogInformation("Profiler is running...");
                myProfilerIsRunning = true;
                myStartProfilingTicks = 0;
            }
        }

        /// <summary>
        /// Stops the profiler measurement and writes results to the trace.
        /// </summary>
        public static void StopProfiler()
        {
            long aStopProfilingTicks = GetTimeTicks();
            string aProfilingDuration = TimeTicksToString(aStopProfilingTicks - myStartProfilingTicks);

            // Wait until all items are processed.
            myQueueThreadEndedEvent.WaitOne();

            lock (myProfilerData)
            {
                if (myProfilerIsRunning)
                {
                    myProfilerIsRunning = false;

                    Logger?.LogInformation(string.Join("", "Number of profiled calls: ", myProfilerCounter));
                    Logger?.LogInformation(string.Join("", "Duration: ", aProfilingDuration));

                    foreach (KeyValuePair<MethodBase, ProfilerData> anItem in myProfilerData.OrderByDescending(x => x.Value.Ticks - x.Value.SubcallsTicks))
                    {
                        string aElapsedTime = TimeTicksToString(anItem.Value.Ticks);
                        long aDeltaElapsedTicks = anItem.Value.Ticks > anItem.Value.SubcallsTicks ? anItem.Value.Ticks - anItem.Value.SubcallsTicks : 0;
                        string aDeltaElapsedTime = TimeTicksToString(aDeltaElapsedTicks);
                        string aTimePerCall = TimeTicksToString((long)Math.Round(((double)aDeltaElapsedTicks) / anItem.Value.Calls));
                        string aPerformance = anItem.Key.GetCustomAttributes<PerformanceCriticalAttribute>(false).Any() ? " !!! " : " ";
                        string aMessage = string.Join("", aDeltaElapsedTime, " ", aElapsedTime, " ", anItem.Value.Calls, "x |", anItem.Value.MaxConcurency, "| #", anItem.Value.MaxRecursion, " ", aTimePerCall, aPerformance, anItem.Key.ReflectedType.FullName, ".", anItem.Key.Name, "\r\n");
                        Logger?.LogInformation(aMessage);
                    }

                    myProfilerData.Clear();

                    Logger?.LogInformation("Profiler has ended.");

                    myProfilerCounter = 0;
                }
            }
        }

        public static ILogger Logger { get; set; } = new TextWriterLogger(null);

        /// <summary>
        /// Sets or gets the detail level of the trace.
        /// </summary>
        public static LogLevel DetailLevel { get; set; } = LogLevel.Information;

        public void Log<TState>(LogLevel logLevel, EventId eventId, TState state, Exception exception, Func<TState, Exception, string> formatter)
        {
            Logger?.Log(logLevel, eventId, state, exception, formatter);
        }

        private static void WriteMessage(StackFrame stack, long timeTicks, TraceEntryType entryType, string message, Exception exception = null, long elapsedTicks = -1)
        {
            try
            {
                int threadId = Thread.CurrentThread.ManagedThreadId;

                Action traceJob = () =>
                {
                    var traceState = new TraceState()
                    {
                        EntryType = entryType,
                        TimeTicks = timeTicks,
                        ThreadId = threadId,
                        Method = stack.GetMethod(),
                        Message = message,
                        ElapsedTicks = elapsedTicks,
                    };

                    LogLevel logLevel;
                    switch (entryType)
                    {
                        case TraceEntryType.Warning:
                            {
                                logLevel = LogLevel.Warning;
                                break;
                            }
                        case TraceEntryType.Error:
                            {
                                logLevel = LogLevel.Critical;
                                break;
                            }
                        case TraceEntryType.Debug:
                        case TraceEntryType.Timing:
                        case TraceEntryType.EnteringMethod:
                        case TraceEntryType.LeavingMethod:
                            {
                                logLevel = LogLevel.Debug;
                                break;
                            }
                        default:
                            {
                                logLevel = LogLevel.Information;
                                break;
                            }
                    }

                    try
                    {
                        Logger.Log(logLevel, new EventId(), traceState, exception, (state, err) =>
                        {
                            string messageToTrace;

                            if (err == null)
                            {
                                messageToTrace = state.ToString();
                            }
                            else
                            {
                                messageToTrace = string.Join("\r\n", state.ToString(), err.GetDetailsFromException());
                            }

                            return messageToTrace;
                        });
                    }
                    catch (Exception err)
                    {
                        string anExceptionDetails = err.GetDetailsFromException();
                        Console.WriteLine("Trace failed to log to logger. " + anExceptionDetails);
                    }
                };

                EnqueueJob(traceJob);
            }
            catch (Exception err)
            {
                // Note: In case the tracing fails, the error should not be propagated to the application.
                //       Therefore, the exception is ignored.
                string anExceptionDetails = err.GetDetailsFromException();
                Console.WriteLine("Trace failed to trace the message. " + anExceptionDetails);
            }
        }

        private static void UpdateProfilerForEntering(Trace trace)
        {
            int aThreadId = Thread.CurrentThread.ManagedThreadId;

            Action aProfilerJob = () =>
            {
                trace.myThreadId = aThreadId;
                MethodBase aMethod = trace.myCallStack.GetMethod();

                //lock (myProfilerData)
                {
                    if (myStartProfilingTicks == 0)
                    {
                        myStartProfilingTicks = GetTimeTicks();
                    }
                    ++myProfilerCounter;

                    ProfilerData aProfileData;
                    myProfilerData.TryGetValue(aMethod, out aProfileData);
                    if (aProfileData == null)
                    {
                        aProfileData = new ProfilerData();
                        aProfileData.Calls = 1;
                        aProfileData.MaxConcurency = 1;
                        aProfileData.MaxRecursion = 1;
                        aProfileData.Threads[aThreadId] = 1;

                        myProfilerData[aMethod] = aProfileData;
                    }
                    else
                    {
                        ++aProfileData.Calls;

                        // If this thread is already inside then it is a recursion.
                        if (aProfileData.Threads.ContainsKey(aThreadId))
                        {
                            int aRecursion = ++aProfileData.Threads[aThreadId];
                            if (aRecursion > aProfileData.MaxRecursion)
                            {
                                aProfileData.MaxRecursion = aRecursion;
                            }
                        }
                        // ... else it is another thread wich is parallel inside.
                        else
                        {
                            aProfileData.Threads[aThreadId] = 1;
                            if (aProfileData.Threads.Count > aProfileData.MaxConcurency)
                            {
                                aProfileData.MaxConcurency = aProfileData.Threads.Count;
                            }
                        }
                    }


                    myActiveStacks.TryGetValue(aThreadId, out Stack<ProfilerData> aStack);
                    if (aStack == null)
                    {
                        aStack = new Stack<ProfilerData>();
                        myActiveStacks[aThreadId] = aStack;
                    }
                    aStack.Push(aProfileData);

                    trace.myBufferedProfileData = aProfileData;
                }
            };

            EnqueueJob(aProfilerJob);
        }

        private static void UpdateProfilerForLeaving(Trace trace, long ticks)
        {
            Action aProfilerJob = () =>
            {
                //lock (myProfilerData)
                {
                    trace.myBufferedProfileData.Ticks += ticks;
                    int aRecursion = --trace.myBufferedProfileData.Threads[trace.myThreadId];

                    if (aRecursion < 1)
                    {
                        MethodBase aMethod = trace.myCallStack.GetMethod();
                        ProfilerData aProfileData = myProfilerData[aMethod];
                        aProfileData.Threads.Remove(trace.myThreadId);
                    }

                    myActiveStacks.TryGetValue(trace.myThreadId, out Stack<ProfilerData> aStack);
                    if (aStack != null)
                    {
                        aStack.Pop();

                        if (aStack.Count > 0)
                        {
                            ProfilerData aCallerProfilerData = aStack.Peek();
                            aCallerProfilerData.SubcallsTicks += ticks;
                        }
                        else
                        {
                            // note: do not remove the empty stack from the dictionary so that it can be reused if needed.
                            //       the dictionary is cleared at the very end of profilling.
                        }
                    }
                }
            };

            EnqueueJob(aProfilerJob);
        }

        private static string TimeTicksToString(long timeTicks)
        {
            DateTime aElapsedTime = DateTime.FromFileTimeUtc(timeTicks);
            string aResult = aElapsedTime.ToString("HH:mm:ss.ffffff");
            return aResult;
        }

        private static long GetTimeTicks()
        {
            long aResult;

            if (UseGetSystemTimePreciseAsFileTime)
            {
                GetSystemTimePreciseAsFileTime(out aResult);
            }
            else
            {
                aResult = Stopwatch.GetTimestamp();
            }

            return aResult;
        }

        /// <summary>
        /// Enqueues a job to the queue.
        /// </summary>
        /// <remarks>
        /// The queueing of jobs ensures, the jobs are performed in the correct order
        /// and the writing of the processing does not consume the execution thread.
        /// </remarks>
        /// <param name="job"></param>
        private static void EnqueueJob(Action job)
        {
            lock (myTraceQueue)
            {
                // If the thread processing messages is not running.
                if (!myProcessingIsRunning)
                {
                    myProcessingIsRunning = true;
                    ThreadPool.QueueUserWorkItem(ProcessJobs);
                }

                // Enqueue the trace message.
                myTraceQueue.Enqueue(job);
            }
        }


        /// <summary>
        /// Removes traces from the queue and writes them.
        /// </summary>
        /// <remarks>
        /// The method is executed from a different thread.
        /// The thread then loops until the queue is processed.
        /// </remarks>
        /// <param name="x"></param>
        private static void ProcessJobs(object x)
        {
            myQueueThreadEndedEvent.Reset();

            ThreadPriority aOriginalPriority = Thread.CurrentThread.Priority;
            try
            {
                Thread.CurrentThread.Priority = ThreadPriority.Lowest;

                while (true)
                {
                    Action aJob;
                    lock (myTraceQueue)
                    {
                        if (myTraceQueue.Count == 0)
                        {
                            myProcessingIsRunning = false;
                            return;
                        }

                        aJob = myTraceQueue.Dequeue();
                    }

                    // Execute the job.
                    try
                    {
                        aJob();
                    }
                    catch (Exception err)
                    {
                        string anExceptionDetails = err.GetDetailsFromException();
                        Console.WriteLine("Trace failed. " + anExceptionDetails);
                    }
                }
            }
            finally
            {
                myQueueThreadEndedEvent.Set();

                Thread.CurrentThread.Priority = aOriginalPriority;
            }
        }
    }
}
