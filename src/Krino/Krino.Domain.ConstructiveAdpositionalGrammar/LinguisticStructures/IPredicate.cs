using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IPredicate : ILinguisticStructure
    {
        List<IPhrase> Phrases { get; }
    }
}
