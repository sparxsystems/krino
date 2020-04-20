using System;
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
        /// Instantiates the enum group.
        /// </summary>
        /// <param name="parent">Parent enum group.</param>
        /// <param name="localPosition">Local position of this group in the parent group.</param>
        /// <param name="groupLength">Max number of enums in this group.</param>
        protected EnumGroupBase(EnumGroupBase parent, int localPosition, int groupLength)
            : base(parent, localPosition)
        {
            // If the group is the root.
            if (parent == null)
            {
                StartPosition = 0;
            }
            // If the parent of this group is the root.
            else if (parent.StartPosition == 0)
            {
                StartPosition = parent.StartPosition + 1 + parent.Length;
            }
            // The group starts behind its parent.
            else
            {
                StartPosition = parent.StartPosition + parent.Length;
            }

            Length = groupLength;

            if (StartPosition > 0 && StartPosition - 1 + Length > sizeof(ulong) * 8 ||
                StartPosition == 0 && StartPosition + Length > sizeof(ulong) * 8)
            {
                throw new ArgumentOutOfRangeException($"Failed to create enum '{GetType().Name}' because its start position {StartPosition} + the group length {Length} exceeds the 64 bit capacity.");
            }
        }


        /// <summary>
        /// Start position of this group.
        /// </summary>
        public int StartPosition { get; private set; }

        /// <summary>
        /// Max number of enums this group can host.
        /// </summary>
        public int Length { get; private set; }
    }
}
