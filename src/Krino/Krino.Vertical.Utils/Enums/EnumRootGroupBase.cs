namespace Krino.Vertical.Utils.Enums
{
    /// <summary>
    /// Declares the root for the structured enum.
    /// </summary>
    /// <remarks>
    /// To declare the root of an enum it is needed to deerive from this class.
    /// </remarks>
    public abstract class EnumRootGroupBase : EnumGroupBase
    {
        public EnumRootGroupBase(int groupLength) : base(null, groupLength, 0) { }
    }
}
