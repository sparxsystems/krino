namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    /// <summary>
    /// Type of the position for the attaching.
    /// </summary>
    public enum AdTreePosition
    {
        /// <summary>
        /// Child on left.
        /// </summary>
        ChildOnLeft,

        /// <summary>
        /// Child on right.
        /// </summary>
        ChildOnRight,

        /// <summary>
        /// Parent for a child on left.
        /// </summary>
        ParrentForChildOnLeft,

        /// <summary>
        /// Parent for child on right.
        /// </summary>
        ParrentForChildOnRight,
    }
}
