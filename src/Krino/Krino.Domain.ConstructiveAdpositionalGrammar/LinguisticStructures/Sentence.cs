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

        public IMorpheme PunctuationMark { get; set; }

        public string Value => string.Join("", string.Join(" ", Clauses.Select(x => x.Value)), PunctuationMark?.Value);

        public string GrammarStr => string.Join("", AttributesStr, "(", string.Join(" ", Clauses.Select(x => x.GrammarStr)), ")");
    }
}
