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
        /// Grammar character.
        /// </summary>
        GrammarCharacter GrammarCharacter { get; set; }

        /// <summary>
        /// Attributes of the morpheme.
        /// </summary>
        Attributes Attributes { get; set; }
    }
}
