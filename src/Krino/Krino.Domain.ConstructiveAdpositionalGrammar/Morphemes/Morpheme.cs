using System.Collections.Generic;
using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    [DebuggerDisplay("{GrammarCharacter}: {Morph}")]
    public class Morpheme : IMorpheme
    {
        public Morpheme (string morph)
        {
            Morph = morph;
        }

        public string Morph { get; private set; }

        public List<ISememe> Sememes { get; } = new List<ISememe>();

        public GrammarCharacterType GrammarCharacter { get; set; }

        public AdpositionType Adposition { get; set; }

        public AttributeTypes Attributes { get; set; }
    }
}
