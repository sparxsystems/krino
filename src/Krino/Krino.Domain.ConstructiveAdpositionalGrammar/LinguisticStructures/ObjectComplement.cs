using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class ObjectComplement : PhraseBase, IObjectComplement
    {
        public ObjectComplement(BigInteger attributes)
            : base(attributes)
        {
        }
    }
}
