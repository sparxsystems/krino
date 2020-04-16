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

        public GrammarCharacter GrammarCharacter { get; set; }

        public AdpositionTypes Adposition { get; set; }

        public Attributes Attributes { get; set; }
    }
}
