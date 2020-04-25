using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;

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
        /// Encodes grammar classification attributes the morphem carries.
        /// </summary>
        ulong Attributes { get; set; }

        /// <summary>
        /// Grammar character.
        /// </summary>
        GrammarCharacter GrammarCharacter { get; }
    }
}
