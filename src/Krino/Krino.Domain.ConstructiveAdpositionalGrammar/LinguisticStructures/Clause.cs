using Krino.Vertical.Utils.Strings;
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

        public string Value => StringExt.JoinIgnoreEmpty(" ", Subject?.Value, Predicate?.Value);
    }
}
