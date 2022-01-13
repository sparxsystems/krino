using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Predicate : LinguisticStructureBase, IPredicate
    {
        public Predicate(BigInteger attributes)
            : base(attributes)
        {
        }

        public List<ITerm> Terms { get; } = new List<ITerm>();


        public string Value => string.Join(" ", Terms.Select(x => x.Value));
    }
}
