using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Objectt : PhraseBase, IObject
    {
        public Objectt(EnumRootBase enumRoot, BigInteger attributes)
            : base(enumRoot, attributes)
        {
        }
    }
}
