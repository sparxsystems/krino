using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Semantics
{
    /// <summary>
    /// Attributes used for adpositions.
    /// </summary>
    [Flags]
    public enum AdpositionAttributeTypes
    {
        None = 0,

        /// <summary>
        /// Adposition filling a structural position.
        /// </summary>
        /// <remarks>
        /// E.g. adposition linking a stative and a verb on a particular valency position.
        /// </remarks>
        Structural,

        /// <summary>
        /// Conjunction.
        /// </summary>
        Conjunction,

        /// <summary>
        /// Preposition.
        /// </summary>
        Preposition,

        /// <summary>
        /// Postpostion.
        /// </summary>
        /// <remarks>
        /// It seems the only english postposition is the word ego.
        /// E.g. Three years ego.
        /// </remarks>
        Postposition,


        InPosition,
    }
}
