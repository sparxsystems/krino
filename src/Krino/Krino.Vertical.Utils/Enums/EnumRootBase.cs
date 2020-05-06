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
    }
}
