using System.Diagnostics;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Abstract class to declare the enum group.
    /// </summary>
    [DebuggerDisplay("{(ulong)this}")]
    public abstract class EnumGroupBase : EnumBase
    {
        /// <summary>
        /// Constructs the enum group.
        /// </summary>
        /// <param name="parent">Parent enum group.</param>
        /// <param name="localPosition">Local position of this group in the parent group.</param>
        /// <param name="groupLength">Max number of enums on the root.</param>
        public EnumGroupBase(EnumGroupBase parent, int localPosition, int groupLength)
            : base(parent != null ? parent : (ulong)0,
                   parent != null ? parent.StartPosition : 0,
                   parent != null ? parent.Length : 0,
                   localPosition, groupLength)
        { }

    }
}
