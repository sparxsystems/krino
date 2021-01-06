using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    [DebuggerDisplay("{DebuggerDisplay}")]
    public class Morpheme : IEquatable<Morpheme>
    {
        /// <summary>
        /// Indicates there is no morpheme.
        /// </summary>
        /// <remarks>
        /// This is used by zero-marked adpositions.
        /// </remarks>
        public static Morpheme Epsilon => new Morpheme("", AttributesArrangement.Attributes.Epsilon);

        public Morpheme (string morph, BigInteger attributes)
        {
            Morph = morph;
            Attributes = attributes;
        }

        public string Morph { get; private set; }

        public BigInteger Attributes { get; private set; }

        public GrammarCharacter GrammarCharacter => GrammarCharacterExt.GetGrammarCharacter(Attributes);

        /// <summary>
        /// Returns true if the morpheme is a lexeme.
        /// </summary>
        /// <remarks>
        /// It does not have to be lexeme nor non-lexeme.
        /// </remarks>
        public bool IsLexeme => AttributesArrangement.Attributes.IsLexeme(Attributes);

        /// <summary>
        /// Returns true if the morpheme is a non-lexeme.
        /// </summary>
        /// <remarks>
        /// It does not have to be lexeme nor non-lexeme.
        /// </remarks>
        public bool IsNonLexeme => AttributesArrangement.Attributes.IsNonLexeme(Attributes);

        public bool Equals(Morpheme other) => Morph == other.Morph && Attributes == other.Attributes;

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ Morph.GetHashCode();
            hash = (hash * 16777619) ^ Attributes.GetHashCode();

            return hash;
        }

        private string DebuggerDisplay => string.Join("", GrammarCharacter, ": ", Morph);
    }
}
