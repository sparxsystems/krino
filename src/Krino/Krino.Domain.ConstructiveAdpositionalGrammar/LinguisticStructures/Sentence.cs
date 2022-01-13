using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Sentence : LinguisticStructureBase, ISentence
    {
        public Sentence(BigInteger attributes)
            : base(attributes)
        {
        }

        public List<IClause> Clauses { get; } = new List<IClause>();

        public string Value => string.Join(" ", Clauses.Select(x => x.Value));
    }
}
