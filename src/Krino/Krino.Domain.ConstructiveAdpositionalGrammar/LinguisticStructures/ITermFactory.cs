using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface ITermFactory
    {
        ITerm Create(IAdTree adTree);
    }
}
