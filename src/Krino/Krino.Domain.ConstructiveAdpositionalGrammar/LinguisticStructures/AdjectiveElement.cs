using Krino.Vertical.Utils.Enums;
using System.Numerics;
namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class AdjectiveElement : PhraseBase, IAdjectiveElement
    {
        public AdjectiveElement(EnumRootBase enumRoot, BigInteger attributes)
            : base(enumRoot, attributes)
        {
        }
    }
}
