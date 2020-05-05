using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Numerics;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Represents a group in a structured enum.
    /// </summary>
    /// <remarks>
    /// This class is intended to derive from when a group is needed in the structured enum.
    /// </remarks>
    [DebuggerDisplay("{(System.Numerics.BigInteger)this}")]
    public abstract class EnumBase
    {
        private int myBitIndex;

        /// <summary>
        /// Instantiates the enum.
        /// </summary>
        /// <param name="parent">Parent enum group.</param>
        /// <param name="globalIndex">Index within the whole bit array.</param>
        protected EnumBase(EnumGroupBase parent)
        {
            if (parent != null)
            {
                ParentEnum = parent;

                // Ensure the root is properly defined.
                EnumRootBase root = ParentEnums.OfType<EnumRootBase>().LastOrDefault();
                if (root == null)
                {
                    throw new InvalidOperationException($"Could not instantiate the enum because the root enum of type {nameof(EnumRootBase)} is missing.");
                }

                myBitIndex = root.Length;

                // Increase number of registered enums.
                ++root.Length;
            }
        }

        /// <summary>
        /// Returns the parent enum group f this enum.
        /// </summary>
        public EnumGroupBase ParentEnum { get; private set; }

        /// <summary>
        /// Returns the sequence of parents to the root.
        /// </summary>
        public IEnumerable<EnumGroupBase> ParentEnums
        {
            get
            {
                EnumGroupBase parent = ParentEnum;
                while (parent != null)
                {
                    yield return parent;
                    parent = parent.ParentEnum;
                }
            }
        }

        /// <summary>
        /// Returns the root 
        /// </summary>
        public EnumRootBase EnumRoot { get { return ParentEnum == null ? this as EnumRootBase : ParentEnums.OfType<EnumRootBase>().Last(); } }


        /// <summary>
        /// Returns the value of this enum.
        /// </summary>
        public BigInteger Value
        {
            get
            {
                byte[] valueInBytes = GetValueInBytes();
                BigInteger result = new BigInteger(valueInBytes);
                return result;
            }
        }


        /// <summary>
        /// True if the value encodes this enum.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public bool IsIn(BigInteger value) => IsIn(Value, value);

        private byte[] GetValueInBytes()
        {
            byte[] result;

            // If this is root.
            if (this is EnumRootBase root)
            {
                int arrayLength = root.Length % 8 == 0 ? 1 + root.Length / 8 : 1 + root.Length / 8 + 1;
                result = new byte[arrayLength];
            }
            else
            {
                result = ParentEnum.GetValueInBytes();

                // Note: the most significant byte is at the end so count the bit position from the end of the array.
                //       and to get the unsigned number the very last byte stays 00.
                result[result.Length - myBitIndex / 8 - 2] |= (byte)(0x80 >> (myBitIndex % 8));
            }

            return result;
        }

        /// <summary>
        /// True if the tested value encodes the expected value.
        /// </summary>
        /// <param name="expected"></param>
        /// <param name="testedValue"></param>
        /// <returns></returns>
        public static bool IsIn(BigInteger expected, BigInteger testedValue) => (expected & testedValue) == expected;


        /// <summary>
        /// Implicitly converts the enum into the ulong.
        /// </summary>
        /// <param name="attribute"></param>
        public static implicit operator BigInteger(EnumBase attribute) => attribute.Value;

        public static BigInteger operator |(EnumBase item1, EnumBase item2) => item1.Value | item2.Value;

        public static BigInteger operator &(EnumBase item1, EnumBase item2) => item1.Value | item2.Value;

        public static BigInteger operator %(EnumBase item1, EnumBase item2) => item1.Value % item2.Value;

        public static bool operator >(EnumBase item1, EnumBase item2) => item1.Value > item2.Value;

        public static bool operator >=(EnumBase item1, EnumBase item2) => item1.Value >= item2.Value;

        public static bool operator <(EnumBase item1, EnumBase item2) => item1.Value < item2.Value;

        public static bool operator <=(EnumBase item1, EnumBase item2) => item1.Value <= item2.Value;
    }
}
