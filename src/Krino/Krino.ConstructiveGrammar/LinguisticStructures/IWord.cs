using System.Collections.Generic;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public interface IWord : IPhraseItem, ILinguisticStructure
    {
        List<IMorpheme> Prefixes { get; }

        List<IMorpheme> Roots { get; set; }

        List<IMorpheme> Suffixes { get; }

        IEnumerable<IMorpheme> Morphemes { get; }

        bool CanBind(IMorpheme morpheme);
        void Bind(IMorpheme morpheme);
    }
}
