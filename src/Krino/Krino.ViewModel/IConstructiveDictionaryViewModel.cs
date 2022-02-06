using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;

namespace Krino.ViewModel
{
    public interface IConstructiveDictionaryViewModel
    {
        string GetAdTreeVisualization(IAdTree adTree);
    }
}
