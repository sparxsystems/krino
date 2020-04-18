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

        public ulong Attributes { get; set; }
    }
}
