using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributes;
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

                if (StructAttributes.Verb.IsIn(Attributes) ||
                    StructAttributes.Interjection.IsIn(Attributes))
                {
                    result = GrammarCharacterType.I;
                }
                else if (StructAttributes.Noun.IsIn(Attributes) ||
                         StructAttributes.Pronoun.IsIn(Attributes))
                {
                    result = GrammarCharacterType.O;
                }
                else if (StructAttributes.Adjective.IsIn(Attributes) ||
                         StructAttributes.Determiner.IsIn(Attributes) ||
                         StructAttributes.Numeral.IsIn(Attributes))
                {
                    result = GrammarCharacterType.A;
                }
                else if (StructAttributes.Adverb.IsIn(Attributes))
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
