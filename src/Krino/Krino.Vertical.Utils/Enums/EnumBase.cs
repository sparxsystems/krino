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

        protected EnumBase(ulong parentMask, int parentStartPosition, int parentLength, int localPositionInParent, int length)
        {
            // If it is the root.
            if (parentLength == 0)
            {
                StartPosition = 0;
                Length = length;
                myValue = 0;
            }
            else
            {
                if (localPositionInParent > parentLength)
                {
                    throw new ArgumentOutOfRangeException($"Failed to add element into '{parentMask.GetType()}' because the position {localPositionInParent} exceeds the group length {parentLength}.");
                }

                StartPosition = parentStartPosition + parentLength;
                Length = length;
                myValue = parentMask | (((ulong)1) << (parentStartPosition + localPositionInParent - 1));
            }

            if (StartPosition + Length > 64)
            {
                throw new ArgumentOutOfRangeException($"Failed to add element into '{parentMask.GetType()}' because the starting position {StartPosition} + the group length {Length} exceeds the 64 bit capacity of the ulong type.");
            }
        }

        public int StartPosition { get; private set; }

        public int Length { get; private set; }


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
