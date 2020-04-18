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
    public abstract class EnumGroupBase
    {
        private ulong myValue;
        private int myStartPosition;
        private int myGroupLength;

        protected EnumGroupBase(EnumGroupBase parent, int groupLength,  int localPosition)
        {
            if (parent != null)
            {
                if (localPosition > parent.myGroupLength)
                {
                    throw new ArgumentOutOfRangeException($"Failed to add element into '{parent.GetType()}' because the position {localPosition} exceeds the group length {parent.myGroupLength}.");
                }

                myValue = parent | (((ulong)1) << (parent.myStartPosition + localPosition - 1));

                myStartPosition = parent.myStartPosition + parent.myGroupLength;
                myGroupLength = groupLength;
            }
            else
            {
                myStartPosition = 0;
                myGroupLength = groupLength;
            }

            if (myStartPosition + myGroupLength > 64)
            {
                throw new ArgumentOutOfRangeException($"Failed to add element into '{parent.GetType()}' because the starting position {myStartPosition} + the group length {myGroupLength} exceeds the 64 bit capacity of the ulong type.");
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
        public static implicit operator ulong(EnumGroupBase attribute) => attribute.myValue;
    }
}
