using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IPredicate : IPhraseItem, ILinguisticStructure
    {
        List<IPhrase> Phrases { get; }
    }
}
