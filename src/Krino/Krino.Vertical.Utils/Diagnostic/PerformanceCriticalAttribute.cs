using System;

namespace Krino.Vertical.Utils.Diagnostic
{
    /// <summary>
    /// Indicates the method can have a significant performance impact.
    /// </summary>
    [AttributeUsage(AttributeTargets.Method | AttributeTargets.Property | AttributeTargets.Constructor, AllowMultiple = false)]
    public class PerformanceCriticalAttribute : Attribute
    {
        public string Info { get; set; }
    }
}
