using System.Collections.Generic;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures
{
    public interface IText : ILinguisticStructure
    {
        List<ISentence> Sentences { get; }
    }
}
