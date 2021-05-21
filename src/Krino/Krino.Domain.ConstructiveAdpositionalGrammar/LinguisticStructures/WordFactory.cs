using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public class WordFactory : IWordFactory
    {
        public IWord Create(IAdTree adTree) => new Word(adTree);
    }
}
