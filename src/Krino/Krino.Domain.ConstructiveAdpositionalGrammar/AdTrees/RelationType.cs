namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    /// <summary>
    /// Indicates the information prominence direction between governor and dependent.
    /// </summary>
    public enum RelationType
    {
        /// <summary>
        /// The information prominence direction is both, irrelevant or unnown.
        /// </summary>
        Generic,

        /// <summary>
        /// The information prominence goes from the governor to the dependent.
        /// </summary>
        Government,

        /// <summary>
        /// The information prominence goes from the dependent to the governor.
        /// </summary>
        Dependency
    }
}
