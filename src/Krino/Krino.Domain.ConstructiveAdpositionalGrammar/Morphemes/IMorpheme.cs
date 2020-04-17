using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Semantics;
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
        /// Attributes in case the morpheme is a stative.
        /// </summary>
        StativeAttributeTypes StativeAttributes { get; set; }

        /// <summary>
        /// Attributes in case the morpheme is a verb.
        /// </summary>
        VerbAttributeTypes VerbAttributes { get; set; }

        /// <summary>
        /// Attributes in case the morpheme is an adjunctive.
        /// </summary>
        AdjunctiveAttributeTypes AdjunctiveAttributes { get; set; }

        /// <summary>
        /// Attributes in case the morpheme is an adposition.
        /// </summary>
        AdpositionAttributeTypes AdpositionAttributes { get; set; }
    }
}
