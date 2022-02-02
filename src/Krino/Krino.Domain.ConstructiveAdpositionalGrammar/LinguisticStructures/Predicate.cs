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

        public List<IPhrase> Phrases { get; } = new List<IPhrase>();


        public string Value => string.Join(" ", Phrases.Select(x => x.Value));
    }
}
