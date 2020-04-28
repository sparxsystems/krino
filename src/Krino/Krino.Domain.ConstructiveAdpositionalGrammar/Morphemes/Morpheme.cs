using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    [DebuggerDisplay("{GrammarCharacter}: {Morph}")]
    public class Morpheme : IMorpheme
    {
        public Morpheme (string morph)
        {
            Morph = morph;

            // Initialize attributes to Epsilon.
            Attributes = StructuralAttributes.Epsilon;
        }

        public string Morph { get; private set; }

        public GrammarCharacter GrammarCharacter => StructuralAttributes.GetGrammarCharacter(Attributes);

        public ulong Attributes { get; set; }
    }
}
