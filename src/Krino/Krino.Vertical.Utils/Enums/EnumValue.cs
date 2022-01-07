using System.Numerics;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Represents a value (in a structured enum) which is not a group just value.
    /// </summary>
    public class EnumValue : EnumBase
    {
        public EnumValue(EnumGroupBase parent)
            : base(parent) { }

        /// <summary>
        /// Clears bit related to this value.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public BigInteger Clear(BigInteger value)
        {
            var bytes = value.ToByteArray();
            bytes[bytes.Length - BitIndex / 8 - 2] &= (byte)(~(0x80 >> (BitIndex % 8)));
            BigInteger result = new BigInteger(bytes);
            return result;
        }
    }
}
