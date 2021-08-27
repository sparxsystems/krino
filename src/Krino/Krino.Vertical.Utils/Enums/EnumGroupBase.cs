using System.Collections.Generic;
using System.Linq;
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

        /// <summary>
        /// Number of bits occupied by this enum group.
        /// </summary>
        internal int Length { get; set; }

        /// <summary>
        /// Recursively returns all enum values for this enum group.
        /// </summary>
        public IEnumerable<EnumValue> EnumValues
        {
            get
            {
                var enumProperties = GetType().GetProperties().Where(x => typeof(EnumBase).IsAssignableFrom(x.PropertyType));
                foreach (var enumProperty in enumProperties)
                {
                    var enumPropertyValue = enumProperty.GetValue(this);
                    if (enumPropertyValue is EnumRootBase == false && enumPropertyValue is EnumGroupBase enumGroup)
                    {
                        var enumValues = enumGroup.EnumValues;
                        foreach (var enumValue in enumValues)
                        {
                            yield return enumValue;
                        }
                    }
                    else if (enumPropertyValue is EnumValue)
                    {
                        yield return (EnumValue)enumPropertyValue;
                    }
                }
            }
        }

        /// <summary>
        /// Finds all enums applicable for the value.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public IEnumerable<EnumBase> FindEnums(BigInteger value)
        {
            var enumProperties = GetType().GetProperties().Where(x => typeof(EnumBase).IsAssignableFrom(x.PropertyType));
            foreach (var enumProperty in enumProperties)
            {
                var enumPropertyValue = enumProperty.GetValue(this);
                if (enumPropertyValue is EnumRootBase == false && enumPropertyValue is EnumGroupBase enumGroup)
                {
                    // If the group of enums contains the value then find biggest ones.
                    if (enumGroup.IsIn(value))
                    {
                        // If there are some sub-enums containing the input value.
                        var enumValues = enumGroup.FindEnums(value);
                        if (enumValues.Any())
                        {
                            foreach (var enumValue in enumValues)
                            {
                                yield return enumValue;
                            }
                        }
                        // If this is that biggest value.
                        else
                        {
                            yield return enumGroup;
                        }
                    }
                }
                else if (enumPropertyValue is EnumValue enumValue)
                {
                    if (enumValue.IsIn(value))
                    {
                        yield return enumValue;
                    }
                }
            }
        }

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
