using System.Diagnostics;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Represents a value (in a structured enum) which is not a group just value.
    /// </summary>
    [DebuggerDisplay("{(ulong)this}")]
    public class EnumValue : EnumBase
    {
        public EnumValue(EnumBase parent, int localPosition)
            : base(parent,
                   parent != null ? parent.StartPosition : 0,
                   parent != null ? parent.Length : 0,
                   localPosition,
                   0) { }
    }
}
