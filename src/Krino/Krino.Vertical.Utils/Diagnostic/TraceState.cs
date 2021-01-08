using System.Reflection;

namespace Krino.Vertical.Utils.Diagnostic
{
    internal struct TraceState
    {
        private const string ENTERING = "-->";
        private const string LEAVING = "<--";
        private const string TIMING = " T:";
        private const string INFO = " I:";
        private const string WARNING = " W:";
        private const string ERROR = " E:";
        private const string DEBUG = " D:";

        public TraceEntryType EntryType { get; set; }
        public long TimeTicks { get; set; }
        public int ThreadId { get; set; }
        public MethodBase Method { get; set; }

        public string Message { get; set; }

        public long ElapsedTicks { get; set; }

        public override string ToString()
        {
            string result;

            string timeStr = TimeTicks > 0 ? DateTimeExt.TimeTicksToUtcTime(TimeTicks).ToLocalTime().ToString("HH:mm:ss.ffffff") : null;

            if (EntryType == TraceEntryType.LeavingMethod)
            {
                string elapsedTicksStr = (ElapsedTicks > -1) ? DateTimeExt.TimeTicksToUtcTime(ElapsedTicks).ToString("HH:mm:ss.ffffff") : null;

                if (string.IsNullOrEmpty(Message))
                {
                    result = string.Join("", timeStr, " ~", ThreadId, " ", LEAVING, " ", Method?.ReflectedType.FullName, ".", Method?.Name, " [", elapsedTicksStr, "]");
                }
                else
                {
                    result = string.Join("", timeStr, " ~", ThreadId, " ", LEAVING, " ", Method?.ReflectedType.FullName, ".", Method?.Name, " ", Message, " [", elapsedTicksStr, "]");
                }
            }
            else
            {
                string prefix;

                switch (EntryType)
                {
                    case TraceEntryType.Warning:
                        {
                            prefix = WARNING;
                            break;
                        }
                    case TraceEntryType.Error:
                        {
                            prefix = ERROR;
                            break;
                        }
                    case TraceEntryType.Debug:
                        {
                            prefix = DEBUG;
                            break;
                        }
                    case TraceEntryType.Timing:
                        {
                            prefix = TIMING;
                            break;
                        }
                    case TraceEntryType.EnteringMethod:
                        {
                            prefix = ENTERING;
                            break;
                        }
                    case TraceEntryType.LeavingMethod:
                        {
                            prefix = LEAVING;
                            break;
                        }
                    default:
                        {
                            prefix = INFO;
                            break;
                        }
                }

                result = string.Join("", timeStr, " ~", ThreadId, " ", prefix, " ", Method?.ReflectedType.FullName, ".", Method?.Name, " ", Message);
            }

            return result;
        }
    }
}
