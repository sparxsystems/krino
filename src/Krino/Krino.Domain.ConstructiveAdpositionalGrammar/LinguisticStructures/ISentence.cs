using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface ISentence : ILinguisticStructure
    {
        List<IClause> Clauses { get; }
    }
}
