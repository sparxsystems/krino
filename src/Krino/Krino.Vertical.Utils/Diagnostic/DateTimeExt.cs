using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Vertical.Utils.Diagnostic
{
    internal static class DateTimeExt
    {
        public static DateTime TimeTicksToUtcTime(long timeTicks)
        {
            DateTime utcTime = DateTime.FromFileTimeUtc(timeTicks);
            return utcTime;
        }
    }
}
