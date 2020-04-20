using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Attributes;
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

        public GrammarCharacterType GrammarCharacter
        {
            get
            {
                GrammarCharacterType result;

                if (MorphemeAttributes.Verb.IsIn(Attributes) ||
                    MorphemeAttributes.Interjection.IsIn(Attributes))
                {
                    result = GrammarCharacterType.I;
                }
                else if (MorphemeAttributes.Noun.IsIn(Attributes) ||
                         MorphemeAttributes.Pronoun.IsIn(Attributes))
                {
                    result = GrammarCharacterType.O;
                }
                else if (MorphemeAttributes.Adjective.IsIn(Attributes) ||
                         MorphemeAttributes.Determiner.IsIn(Attributes) ||
                         MorphemeAttributes.Numeral.IsIn(Attributes))
                {
                    result = GrammarCharacterType.A;
                }
                else if (MorphemeAttributes.Adverb.IsIn(Attributes))
                {
                    result = GrammarCharacterType.E;
                }
                else
                {
                    result = GrammarCharacterType.U;
                }

                return result;
            }
        }

        public ulong Attributes { get; set; }
    }
}
