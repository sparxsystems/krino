using Microsoft.Extensions.Logging;
using System;
using System.IO;
using System.Text;
using System.Threading;

namespace Krino.Vertical.Utils.Diagnostic
{
    public class TextWriterLogger : ILogger
    {
        private const int myTraceBufferCapacity = 10000000;

        private static object myTraceLogLock = new object();
        private object myTraceBufferLock = new object();
        private StringBuilder myTraceBuffer = new StringBuilder(myTraceBufferCapacity);
        private Timer myTraceBufferFlushTimer;

        public TextWriterLogger(TextWriter textWriter)
        {
            myTraceBufferFlushTimer = new Timer(OnFlushTraceBufferTick, null, -1, -1);
            TextWriter = textWriter;
        }

        public IDisposable BeginScope<TState>(TState state) => default;

        public bool IsEnabled(LogLevel logLevel) => true;

        public TextWriter TextWriter { get; private set; }

        
        public void Log<TState>(LogLevel logLevel, EventId eventId, TState state, Exception exception, Func<TState, Exception, string> formatter)
        {
            var message = formatter(state, exception);

            lock (myTraceBufferLock)
            {
                // If the buffer was empty also start the timer processing the buffer.
                bool startTimerFlag = myTraceBuffer.Length == 0;

                // Add the message to the buffer.
                myTraceBuffer.AppendLine(message);

                if (startTimerFlag)
                {
                    // Flush the buffer in the specified time.
                    myTraceBufferFlushTimer.Change(100, -1);
                }
            }
        }


        // Invoked by the timer.
        // Traces are written to the StringBuilder. StringBuilder is flushed once per 50ms.
        private void OnFlushTraceBufferTick(object x)
        {
            StringBuilder aNewBuffer = new StringBuilder(myTraceBufferCapacity);
            StringBuilder aBufferToFlush;

            // Keep the lock for the shortest possible time.
            lock (myTraceBufferLock)
            {
                aBufferToFlush = myTraceBuffer;
                myTraceBuffer = aNewBuffer;
            }

            string aBufferedTraceMessages = aBufferToFlush.ToString();

            // Flush buffered messages to the trace.
            WriteToTrace(aBufferedTraceMessages);
        }

        private void WriteToTrace(string message)
        {
            try
            {
                lock (myTraceLogLock)
                {
                    // If a trace log is set, then write to it.
                    if (TextWriter != null)
                    {
                        TextWriter.Write(message);
                        TextWriter.Flush();
                    }
                    else
                    {
                        // Otherwise write to the debug port.
                        System.Diagnostics.Debug.Write(message);
                    }
                }
            }
            catch (Exception err)
            {
                string anExceptionDetails = err.GetDetailsFromException();
                Console.WriteLine("Trace failed to write to the trace." + anExceptionDetails);
            }
        }
    }
}
