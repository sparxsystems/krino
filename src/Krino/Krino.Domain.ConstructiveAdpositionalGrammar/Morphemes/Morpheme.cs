using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    [DebuggerDisplay("{GrammarCharacter}: {Morph}")]
    public class Morpheme
    {
        public static Morpheme Epsilon => new Morpheme("") { Attributes = AttributesArrangement.Attributes.Epsilon };


        public Morpheme (string morph)
        {
            Morph = morph;

            // Initialize attributes to Epsilon.
            Attributes = AttributesArrangement.Attributes.Epsilon;
        }

        public string Morph { get; private set; }

        public BigInteger Attributes { get; set; }

        public GrammarCharacter GrammarCharacter => GrammarCharacterExt.GetGrammarCharacter(Attributes);

        public bool IsLexeme => AttributesArrangement.Attributes.IsLexeme(Attributes);
    }
}
