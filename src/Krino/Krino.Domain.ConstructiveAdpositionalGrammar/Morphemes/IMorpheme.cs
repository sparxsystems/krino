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
        GrammarCharacterType GrammarCharacter { get; set; }

        /// <summary>
        /// Type of the adposition this morpheme represents.
        /// </summary>
        AdpositionType Adposition { get; set; }

        /// <summary>
        /// Attributes of the morpheme.
        /// </summary>
        AttributeTypes Attributes { get; set; }
    }
}
