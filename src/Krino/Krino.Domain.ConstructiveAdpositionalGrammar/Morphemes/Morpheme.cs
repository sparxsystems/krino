using System.Collections.Generic;
using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    [DebuggerDisplay("{Character}: {Morph}")]
    public class Morpheme : IMorpheme
    {
        public Morpheme (string morph)
        {
            Morph = morph;
        }

        public string Morph { get; private set; }

        public List<ISememe> Sememes { get; } = new List<ISememe>();

        public GrammarCharacter Character { get; set; }

        public Attributes Attributes { get; set; }
    }
}
