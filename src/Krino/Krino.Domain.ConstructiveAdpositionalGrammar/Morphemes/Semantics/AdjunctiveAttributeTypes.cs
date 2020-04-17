using System;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Semantics
{
    /// <summary>
    /// Attributes used for adjunctives.
    /// </summary>
    [Flags]
    public enum AdjunctiveAttributeTypes
    {
        None = 0,

        /// <summary>
        /// Adjective that comes before the noun (e.g. little baby).
        /// </summary>
        Attributive = 1,

        /// <summary>
        /// Adjective that comes after the verb (e.g. I was happy).
        /// </summary>
        Predicative = 2,

        /// <summary>
        /// Adjective that comes after the noun.
        /// </summary>
        Postpositive = 4,
    }
}
