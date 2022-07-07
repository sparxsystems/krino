using System.Numerics;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures
{
    internal class Predicate : VerbElementBase, IVerbElement, IPredicate
    {
        public Predicate(BigInteger attributes)
            : base(attributes)
        {
        }

        protected override PhraseBase FactoryMethod() => new Predicate(Attributes);
    }
}
