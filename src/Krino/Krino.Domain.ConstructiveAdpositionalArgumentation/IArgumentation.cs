using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;

namespace Krino.Domain.ConstructiveAdpositionalArgumentation
{
    public interface IArgumentation
    {
        bool IsArgument(ISentence sentence);

        ArgumentEvaluationResult Evaluate(ISentence argument);
    }
}
