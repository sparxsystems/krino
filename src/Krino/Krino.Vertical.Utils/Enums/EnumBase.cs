using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Numerics;
using System.Reflection;
using System.Text;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Represents a group in a structured enum.
    /// </summary>
    /// <remarks>
    /// This class is intended to derive from when a group is needed in the structured enum.
    /// </remarks>
    [DebuggerDisplay("{DebuggerDisplay}")]
    public abstract class EnumBase
    {
        /// <summary>
        /// Instantiates the enum.
        /// </summary>
        /// <param name="parent">Parent enum group.</param>
        protected EnumBase(EnumGroupBase parent)
        {
            // If this is not the root.
            if (parent != null)
            {
                ParentEnum = parent;

                EnumGroupBase root = ParentEnums.Last();

                BitIndex = root.Length;

                // Increase number of registered enums.
                foreach (EnumGroupBase parentGroup in ParentEnums)
                {
                    ++parentGroup.Length;
                }
            }
        }

        protected int BitIndex { get; private set; }

        /// <summary>
        /// Returns the parent enum group of this enum.
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
        /// Returns the short name of this enum.
        /// </summary>
        /// <remarks>
        /// It returns the property name from the parent which references this enum.
        /// </remarks>
        /// <returns></returns>
        public string GetName()
        {
            string result;

            if (ParentEnum != null)
            {
                var referencingProperty = GetReferencingProperty(ParentEnum, this);
                result = referencingProperty.Name;
            }
            else
            {
                result = GetType().Name;
            }

            return result;
        }

        /// <summary>
        /// Returns the full name of this enum.
        /// </summary>
        /// <returns></returns>
        public string GetFullName()
        {
            var result = new StringBuilder();

            object parentInstance = null;
            var path = ParentEnums.Reverse();
            foreach (var item in path)
            {
                if (item == path.First())
                {
                    // Root.
                    result.Append(item.GetType().Name).Append(".");
                }
                else
                {
                    // Property chain from the root.
                    var referencingProperty = GetReferencingProperty(parentInstance, item);
                    result.Append(referencingProperty.Name).Append(".");
                }

                parentInstance = item;
            }

            // If it is not the root.
            if (parentInstance != null)
            {
                var property = GetReferencingProperty(parentInstance, this);
                result.Append(property.Name);
            }
            // If this is the root.
            else
            {
                result.Append(GetType().Name);
            }

            return result.ToString();
        }

        private PropertyInfo GetReferencingProperty(object instance, object value)
        {
            var properties = instance.GetType().GetProperties();
            var referencingProperty = properties.First(x => x.GetValue(instance) == value);
            return referencingProperty;
        }

        /// <summary>
        /// True if the given value encodes this enum.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public bool IsIn(BigInteger value) => IsIn(Value, value);

        protected BigInteger Value
        {
            get
            {
                byte[] valueInBytes = GetValueInBytes();
                BigInteger result = new BigInteger(valueInBytes);
                return result;
            }
        }

        private byte[] GetValueInBytes()
        {
            byte[] result;

            // If this is root.
            if (ParentEnum == null && this is EnumGroupBase root)
            {
                int arrayLength = root.Length % 8 == 0 ? 1 + root.Length / 8 : 1 + root.Length / 8 + 1;
                result = new byte[arrayLength];
            }
            else
            {
                result = ParentEnum.GetValueInBytes();

                // Note: the most significant byte is at the end so count the bit position from the end of the array.
                //       and to get the unsigned number the very last byte stays 00.
                result[result.Length - BitIndex / 8 - 2] |= (byte)(0x80 >> (BitIndex % 8));
            }

            return result;
        }

        /// <summary>
        /// True if inValue is encoded within bigValue.
        /// </summary>
        /// <param name="inValue"></param>
        /// <param name="bigValue"></param>
        /// <returns></returns>
        public static bool IsIn(BigInteger inValue, BigInteger bigValue) => (inValue & bigValue) == inValue;

        public override string ToString() => GetFullName();
        


        /// <summary>
        /// Implicitly converts the enum into BigInteger.
        /// </summary>
        /// <param name="attribute"></param>
        public static implicit operator BigInteger(EnumBase attribute) => attribute.Value;

        public static BigInteger operator |(EnumBase item1, EnumBase item2) => item1.Value | item2.Value;


        public static bool operator >(EnumBase item1, EnumBase item2) => item1.Value > item2.Value;

        public static bool operator >=(EnumBase item1, EnumBase item2) => item1.Value >= item2.Value;

        public static bool operator <(EnumBase item1, EnumBase item2) => item1.Value < item2.Value;

        public static bool operator <=(EnumBase item1, EnumBase item2) => item1.Value <= item2.Value;

        private string DebuggerDisplay => $"{GetFullName()} = {(BigInteger)this}";
    }
}
