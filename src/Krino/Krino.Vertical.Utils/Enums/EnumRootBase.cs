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
        /// <param name="groupLength">Max number of enums on the root.</param>
        public EnumRootBase(int groupLength) : base(null, 0, groupLength) { }
    }
}
