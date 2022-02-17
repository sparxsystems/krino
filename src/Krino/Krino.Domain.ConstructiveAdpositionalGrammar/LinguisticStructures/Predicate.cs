using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Predicate : VerbElementBase, IVerbElement, IPredicate
    {
        public Predicate(BigInteger attributes)
            : base(attributes)
        {
        }
    }
}
