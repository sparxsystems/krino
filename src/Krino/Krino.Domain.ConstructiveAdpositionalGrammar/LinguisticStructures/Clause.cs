using Krino.Vertical.Utils.Strings;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Clause : LinguisticStructureBase, IClause
    {
        public Clause(BigInteger attributes)
            : base(attributes)
        {
        }

        public IWord Conjunction { get; set; }
        public ISubject Subject { get; set; }
        public IPredicate Predicate { get; set; }

        public IEnumerable<IClause> DependentClauses
        {
            get
            {
                var clausesInSubject = Subject.AllItems.OfType<IClause>();
                var clausesInPredicate = Predicate.AllItems.OfType<IClause>();

                var result = clausesInSubject.Concat(clausesInPredicate);
                return result;
            }
        }

        public string Value => StringExt.JoinIgnoreEmpty(" ", Conjunction?.Value, Subject?.Value, Predicate?.Value);

        public string GrammarStr => string.Join("", AttributesStr, "(", StringExt.JoinIgnoreEmpty(" ", AttributesStr, Conjunction?.GrammarStr, Subject?.GrammarStr, Predicate?.GrammarStr), ")");
    }
}
