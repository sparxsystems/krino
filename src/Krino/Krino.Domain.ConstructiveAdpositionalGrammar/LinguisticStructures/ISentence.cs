using System.Collections.Generic;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures
{
    public interface ISentence : ILinguisticStructure
    {
        List<IClause> IndependentClauses { get; }

        IMorpheme PunctuationMark { get; set; }
    }
}
