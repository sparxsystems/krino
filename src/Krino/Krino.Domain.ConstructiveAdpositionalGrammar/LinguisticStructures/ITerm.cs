using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface ITerm : ILinguisticStructure
    {
        List<IWord> Words { get; }
    }
}
