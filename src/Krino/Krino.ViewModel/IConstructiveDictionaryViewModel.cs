using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using System.Collections.Generic;
using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;

namespace Krino.ViewModel
{
    public interface IConstructiveDictionaryViewModel
    {
        IEnumerable<Morpheme> Lexemes { get; }

        IEnumerable<Morpheme> NonLexemes { get; }

        IEnumerable<Pattern> Patterns { get; }

        IReadOnlyList<IAdTree> GetAdTree(string phrase);

        string GetAdTreeVisualization(IAdTree adTree);
    }
}
