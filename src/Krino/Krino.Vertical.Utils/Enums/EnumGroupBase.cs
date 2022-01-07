using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;

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
        /// Recursively returns all enums (not only EnumValue) of this enum group.
        /// </summary>
        public IEnumerable<EnumBase> Enums
        {
            get
            {
                foreach (var enumItem in DirectSubEnums)
                {
                    if (enumItem is EnumRootBase == false && enumItem is EnumGroupBase enumGroup)
                    {
                        foreach (var subEnumItem in enumGroup.Enums)
                        {
                            yield return subEnumItem;
                        }
                    }

                    yield return enumItem;
                }
            }
        }

        /// <summary>
        /// Recursively returns all enum values for this enum group.
        /// </summary>
        public IEnumerable<EnumValue> EnumValues => Enums.OfType<EnumValue>();
        

        /// <summary>
        /// Returns enums of this enum group.
        /// </summary>
        public IEnumerable<EnumBase> DirectSubEnums
        {
            get
            {
                var enumPropertyValues = GetType().GetProperties()
                    .Where(x => x.DeclaringType != typeof(EnumGroupBase) &&
                            x.DeclaringType != typeof(EnumBase) &&
                            !typeof(EnumRootBase).IsAssignableFrom(x.PropertyType) &&
                            typeof(EnumBase).IsAssignableFrom(x.PropertyType))
                    .Select(x => (EnumBase)x.GetValue(this));

                return enumPropertyValues;
            }
        }

        /// <summary>
        /// Returns true if a sub enum is present in the value.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public bool HasDirectSubEnums(BigInteger value) => DirectSubEnums.Any(x => x.IsIn(value));

        /// <summary>
        /// Finds all enums applicable for the value.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public IEnumerable<EnumBase> FindEnums(BigInteger value)
        {
            // Go via relevant enum properties.
            var relevantEnumPropertyValues = DirectSubEnums
                .Where(x => x.IsIn(value));

            foreach (var enumPropertyValue in relevantEnumPropertyValues)
            {
                // If it is an enum group but it is not the root.
                if (enumPropertyValue is EnumRootBase == false && enumPropertyValue is EnumGroupBase enumGroup)
                {
                    // If there are some sub-enums containing the value.
                    var enumValues = enumGroup.FindEnums(value);
                    if (enumValues.Any())
                    {
                        foreach (var enumValue in enumValues)
                        {
                            yield return enumValue;
                        }
                    }
                    // There are no sub-enums containing the value.
                    else
                    {
                        yield return enumGroup;
                    }
                }
                else
                {
                    yield return enumPropertyValue;
                }
            }
        }

        public string GetFullName(BigInteger value)
        {
            var result = new StringBuilder();

            var thisType = GetType();

            // Go via relevant enum properties.
            var relevantEnumPropertyValues = DirectSubEnums
                .Where(x => x.IsIn(value))
                .ToList();

            // If this is the root.
            if (relevantEnumPropertyValues.Count > 0 && typeof(EnumRootBase).IsAssignableFrom(thisType))
            {
                result.Append(thisType.Name).Append(".");
            }

            if (relevantEnumPropertyValues.Count > 1)
            {
                result.Append("(");
            }

            for (int i = 0; i < relevantEnumPropertyValues.Count; ++i)
            {
                var enumPropertyValue = relevantEnumPropertyValues[i];

                if (i > 0)
                {
                    result.Append(",");
                }

                // If it is an enum group but it is not the root.
                if (enumPropertyValue is EnumRootBase == false && enumPropertyValue is EnumGroupBase enumGroup)
                {
                    var name = enumGroup.GetName();
                    result.Append(name);

                    // If there are some sub-enums containing the value.
                    var subEnumName = enumGroup.GetFullName(value);
                    if (!string.IsNullOrEmpty(subEnumName))
                    {
                        result.Append(".").Append(subEnumName);
                    }
                }
                else
                {
                    var name = enumPropertyValue.GetName();
                    result.Append(name);
                }
            }

            if (relevantEnumPropertyValues.Count > 1)
            {
                result.Append(")");
            }

            return result.ToString();
        }

        /// <summary>
        /// Clears all bits related to this enum group, i.e. this group and all its sub-groups and values.
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


        protected BigInteger GetValueFromPath(string[] path, int idx)
        {
            BigInteger result = 0;

            if (idx < path.Length)
            {
                var enumProperty = GetType().GetProperty(path[idx]);
                if (enumProperty != null)
                {
                    var enumPropertyValue = enumProperty.GetValue(this);
                    if (idx + 1 < path.Length)
                    {
                        if (enumPropertyValue is EnumGroupBase enumGroupBase)
                        {
                            result = enumGroupBase.GetValueFromPath(path, idx + 1);
                        }
                    }
                    else if (enumPropertyValue is EnumBase enumBase)
                    {
                        result = enumBase;
                    }
                }
            }

            return result;
        }

        
    }
}
