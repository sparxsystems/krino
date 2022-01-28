using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Clause : LinguisticStructureBase, IClause
    {
        public Clause(EnumRootBase enumRoot, BigInteger attributes)
            : base(enumRoot, attributes)
        {
        }

        public ISubject Subject { get; set; }

        public IPredicate Predicate { get; set; }

        public string Value => string.Join(" ", Subject.Value, Predicate.Value);
    }
}
