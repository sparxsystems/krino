using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalArgumentation
{
    public class ArgumentEvaluationResult
    {
        public IReadOnlyCollection<ISentence> Lever { get; private set; }
        public bool IsCorrect { get; private set; }
    }
}
