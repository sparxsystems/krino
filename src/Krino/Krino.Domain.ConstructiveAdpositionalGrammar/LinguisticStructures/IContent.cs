using System.Collections.Generic;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures
{
    public interface IContent : ILinguisticStructure
    {
        IEnumerable<ISentence> Sentences { get; }
    }
}
