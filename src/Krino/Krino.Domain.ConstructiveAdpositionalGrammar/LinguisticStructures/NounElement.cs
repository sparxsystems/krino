using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class NounElement : PhraseBase, INounElement
    {
        public NounElement(EnumRootBase enumRoot, BigInteger attributes)
            : base(enumRoot, attributes)
        {
        }
    }
}
