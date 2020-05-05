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
        /// <param name="localPosition">Local position of this group in the parent group.</param>
        /// <param name="groupLength">Max number of enums in this group.</param>
        protected EnumGroupBase(EnumGroupBase parent)
            : base(parent)
        {

        }
    }
}
