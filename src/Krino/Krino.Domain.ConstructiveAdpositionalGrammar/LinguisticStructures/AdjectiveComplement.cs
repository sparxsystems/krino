using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class AdjectiveComplement : PhraseBase, IAdverbialComplement
    {
        public AdjectiveComplement(BigInteger attributes)
            : base(attributes)
        {
        }
    }
}
