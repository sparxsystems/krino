using System.Collections.Generic;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public interface ISentence : ILinguisticStructure
    {
        List<IClause> IndependentClauses { get; }

        IMorpheme PunctuationMark { get; set; }
    }
}
