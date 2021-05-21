using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public interface IWordFactory
    {
        IWord Create(IAdTree adTree);
    }
}
