using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IPhrase : IPhraseItem, ILinguisticStructure
    {
        List<IPhraseItem> Items { get; }
    }
}
