using System;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    [Flags]
    public enum AdTreeElementType
    {
        /// <summary>
        /// Adposition. (has governor and dependent)
        /// </summary>
        AdPosition = 1,

        /// <summary>
        /// Governor (right branch).
        /// </summary>
        Governor = 2,

        /// <summary>
        /// Dependent (left branch)
        /// </summary>
        Dependent = 4,
    }
}
