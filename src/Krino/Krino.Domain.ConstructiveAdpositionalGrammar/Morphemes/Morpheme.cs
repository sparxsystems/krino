using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Semantics;
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

        public GrammarCharacterType GrammarCharacter { get; set; }

        public StativeAttributeTypes StativeAttributes { get; set; }

        public VerbAttributeTypes VerbAttributes { get; set; }

        public AdjunctiveAttributeTypes AdjunctiveAttributes { get; set; }

        public AdpositionAttributeTypes AdpositionAttributes { get; set; }
    }
}
