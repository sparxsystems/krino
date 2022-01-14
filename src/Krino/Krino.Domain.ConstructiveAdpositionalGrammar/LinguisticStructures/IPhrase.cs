using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IPhrase : ILinguisticStructure
    {
        List<IWord> Words { get; }
    }
}
