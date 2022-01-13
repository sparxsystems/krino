using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IText : ILinguisticStructure
    {
        List<ISentence> Sentences { get; }
    }
}
