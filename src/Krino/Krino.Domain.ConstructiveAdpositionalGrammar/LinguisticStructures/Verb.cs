using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Verb : PhraseBase, IVerb
    {
        public Verb(EnumRootBase enumRoot, BigInteger attributes)
            : base(enumRoot, attributes)
        {
        }
    }
}
