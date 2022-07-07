using System.Collections.Generic;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public interface IWord : IPhraseItem, ILinguisticStructure
    {
        List<IMorpheme> Prefixes { get; }

        IMorpheme Root { get; set; }

        List<IMorpheme> Suffixes { get; }

        IEnumerable<IMorpheme> Morphemes { get; }
    }
}
