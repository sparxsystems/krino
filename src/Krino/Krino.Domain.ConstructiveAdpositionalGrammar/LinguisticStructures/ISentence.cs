using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface ISentence : ILinguisticStructure
    {
        IReadOnlyList<IClause> Clauses { get; }
    }
}
