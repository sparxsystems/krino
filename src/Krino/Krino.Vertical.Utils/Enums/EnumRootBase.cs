using System.Numerics;

namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Abstract class to declare the root of the structured enum.
    /// </summary>
    public abstract class EnumRootBase : EnumGroupBase
    {
        /// <summary>
        /// Constructs the root of the structured enum.
        /// </summary>
        protected EnumRootBase() : base(null) { }

        /// <summary>
        /// Number of bits the whole enum occupies.
        /// </summary>
        new public int Length { get; internal set; }

        /// <summary>
        /// Returns value for the structured enum name.
        /// </summary>
        /// <param name="structuredEnumName"></param>
        /// <param name="value">out parameter value</param>
        /// <returns>true if the enam name was successfully resolved.</returns>
        public bool TryGetValue(string structuredEnumName, out BigInteger value)
        {
            var enumStructure = structuredEnumName.Split('.', System.StringSplitOptions.RemoveEmptyEntries);
            var idx = this is EnumRootBase ? 1 : 0;
            var result = TryGetValue(enumStructure, idx, out value);
            return result;
        }
    }
}
