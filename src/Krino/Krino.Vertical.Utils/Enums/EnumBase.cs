using System;
using System.Diagnostics;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Represents a group in a structured enum.
    /// </summary>
    /// <remarks>
    /// This class is intended to derive from when a group is needed in the structured enum.
    /// </remarks>
    [DebuggerDisplay("{(ulong)this}")]
    public abstract class EnumBase
    {
        private const ulong HIGHEST_BIT = 0x80_00_00_00_00_00_00_00;
        private ulong myValue;

        /// <summary>
        /// Instantiates the enum.
        /// </summary>
        /// <param name="parent">Parent enum group.</param>
        /// <param name="localPositionInParent">Position within the parent enum.</param>
        protected EnumBase(EnumGroupBase parent, int localPositionInParent)
        {
            if (parent != null)
            {
                if (localPositionInParent < 1)
                {
                    throw new ArgumentOutOfRangeException($"Failed to add enum into '{parent.GetType().Name}' because the position {localPositionInParent} is less than 1.");
                }

                if (localPositionInParent > parent.Length)
                {
                    throw new ArgumentOutOfRangeException($"Failed to add enum into '{parent.GetType().Name}' because the position {localPositionInParent} exceeds the group length {parent.Length}.");
                }

                // Note: in case the parent is the root then its startPosition as well as groupLength is 0.
                int parentStartIndex = parent.StartPosition > 0 ? parent.StartPosition - 1 : 0;
                int localIndexInParent = localPositionInParent - 1;

                myValue = parent | (HIGHEST_BIT >> (parentStartIndex + localIndexInParent));
            }
        }


        /// <summary>
        /// True if the value encodes this enum.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public bool IsIn(ulong value) => (value & myValue) == myValue;
        
        /// <summary>
        /// Implicitly converts the enum into the ulong.
        /// </summary>
        /// <param name="attribute"></param>
        public static implicit operator ulong(EnumBase attribute) => attribute.myValue;
    }
}
