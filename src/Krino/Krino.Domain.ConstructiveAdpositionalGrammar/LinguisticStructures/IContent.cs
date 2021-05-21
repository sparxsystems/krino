using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IContent : ILinguisticStructure
    {
        IEnumerable<ISentence> Sentences { get; }
    }
}
