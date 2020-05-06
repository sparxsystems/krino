using System.Numerics;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Abstract class to declare the enum group.
    /// </summary>
    public abstract class EnumGroupBase : EnumBase
    {
        /// <summary>
        /// Instantiates the enum group.
        /// </summary>
        /// <param name="parent">Parent enum group.</param>
        protected EnumGroupBase(EnumGroupBase parent)
            : base(parent)
        {

        }

        internal int Length { get; set; }

        /// <summary>
        /// Clears all bits related to this group.
        /// </summary>
        public BigInteger Clear(BigInteger value)
        {
            BigInteger result;

            // If this is not the root.
            if (ParentEnum != null)
            {
                byte[] bytes = value.ToByteArray();

                for (int i = BitIndex; i < BitIndex + Length + 1; ++i)
                {
                    bytes[bytes.Length - i / 8 - 2] &= (byte)(~(0x80 >> (i % 8)));
                }

                result = new BigInteger(bytes);
            }
            else
            {
                result = new BigInteger(new byte[Length]);
            }

            return result;
        }
    }
}
