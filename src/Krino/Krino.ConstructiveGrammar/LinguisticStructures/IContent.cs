using System.Collections.Generic;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public interface IContent : ILinguisticStructure
    {
        IEnumerable<ISentence> Sentences { get; }
    }
}
