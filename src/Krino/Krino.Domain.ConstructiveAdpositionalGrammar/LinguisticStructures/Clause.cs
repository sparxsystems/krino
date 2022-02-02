using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Clause : LinguisticStructureBase, IClause
    {
        public Clause(BigInteger attributes)
            : base(attributes)
        {
        }

        public ISubject Subject { get; set; }

        public IPredicate Predicate { get; set; }

        public string Value => string.Join(" ", Subject.Value, Predicate.Value);
    }
}
