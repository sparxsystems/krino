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
        /// Grammar character.
        /// </summary>
        GrammarCharacterType GrammarCharacter { get; set; }

        /// <summary>
        /// Encodes grammar classification attributes the morphem carries.
        /// </summary>
        ulong Attributes { get; set; }
    }
}
