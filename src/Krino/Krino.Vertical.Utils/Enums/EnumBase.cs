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
                if (localPositionInParent > parent.Length)
                {
                    throw new ArgumentOutOfRangeException($"Failed to add element into '{parent.GetType()}' because the position {localPositionInParent} exceeds the group length {parent.Length}.");
                }

                myValue = parent | (((ulong)1) << (parent.StartPosition + localPositionInParent - 1));
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
