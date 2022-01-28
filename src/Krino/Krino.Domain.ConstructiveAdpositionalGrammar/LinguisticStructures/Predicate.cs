using Krino.Vertical.Utils.Enums;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Predicate : LinguisticStructureBase, IPredicate
    {
        public Predicate(EnumRootBase enumRoot, BigInteger attributes)
            : base(enumRoot, attributes)
        {
        }

        public List<IPhrase> Phrases { get; } = new List<IPhrase>();


        public string Value => string.Join(" ", Phrases.Select(x => x.Value));
    }
}
