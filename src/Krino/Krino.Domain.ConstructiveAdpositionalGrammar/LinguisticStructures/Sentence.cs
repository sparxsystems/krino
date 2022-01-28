using Krino.Vertical.Utils.Enums;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Sentence : LinguisticStructureBase, ISentence
    {
        public Sentence(EnumRootBase enumRoot, BigInteger attributes)
            : base(enumRoot, attributes)
        {
        }

        public List<IClause> Clauses { get; } = new List<IClause>();

        public string Value => string.Join(" ", Clauses.Select(x => x.Value));
    }
}
