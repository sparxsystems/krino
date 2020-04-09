using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    /// <summary>
    /// Declares the pattern which gives the form to the linguistic construction.
    /// </summary>
    public interface IPattern
    {
        /// <summary>
        /// Returns the sequence of morphemes to recognize the pattern.
        /// </summary>
        IReadOnlyList<IMorpheme> Match { get; }

        /// <summary>
        /// The AdTree representing the pattern rule.
        /// </summary>
        IAdTree Rule { get; }
    }
}
