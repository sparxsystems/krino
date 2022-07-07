using System.Collections.Generic;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public interface IText : ILinguisticStructure
    {
        List<ISentence> Sentences { get; }
    }
}
