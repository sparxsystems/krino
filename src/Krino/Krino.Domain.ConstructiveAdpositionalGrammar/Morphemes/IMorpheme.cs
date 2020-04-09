using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    /// <summary>
    /// Declares the morpheme.
    /// </summary>
    public interface IMorpheme
    {
        /// <summary>
        /// Morph.
        /// </summary>
        string Morph { get; }

        /// <summary>
        /// List of sememe trees.
        /// </summary>
        List<ISememe> Sememes { get; }

        /// <summary>
        /// Attributes of the morpheme.
        /// </summary>
        Dictionary<string, Attribute> Attributes { get; }
    }
}
