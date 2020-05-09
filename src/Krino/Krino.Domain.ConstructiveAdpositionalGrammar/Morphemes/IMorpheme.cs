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
        /// Structural and semantic attributes the morphem carries.
        /// </summary>
        BigInteger Attributes { get; set; }

        /// <summary>
        /// Returns the grammar character of the morpheme.
        /// </summary>
        GrammarCharacter GrammarCharacter { get; }

        /// <summary>
        /// Returns true if the morpheme is a lexeme.
        /// </summary>
        bool IsLexeme { get; }
    }
}
