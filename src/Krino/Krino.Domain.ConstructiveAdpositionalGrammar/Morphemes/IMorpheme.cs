using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using System.Numerics;

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
        /// Encodes structural attributes the morphem carries.
        /// </summary>
        BigInteger Attributes { get; set; }

        /// <summary>
        /// Encodes semantic attributes the morpheme carries.
        /// </summary>
        BigInteger SememeAttributes { get; set; }

        /// <summary>
        /// Returns the grammar character of the morpheme.
        /// </summary>
        GrammarCharacter GrammarCharacter { get; }
    }
}
