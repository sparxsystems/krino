using System.Diagnostics;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Represents a value (in a structured enum) which is not a group just value.
    /// </summary>
    [DebuggerDisplay("{(ulong)this}")]
    public class EnumValue : EnumGroupBase
    {
        public EnumValue(EnumGroupBase parent, int localPosition) : base(parent, 0, localPosition) { }
    }
}
