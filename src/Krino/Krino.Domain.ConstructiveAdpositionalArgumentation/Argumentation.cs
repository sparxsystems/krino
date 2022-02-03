using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalArgumentation
{
    public class Argumentation : IArgumentation
    {
        public bool IsArgument(ISentence sentence) => GrammarAttributes.Sentence.Complex.Argument.IsIn(sentence.Attributes);


        public ArgumentEvaluationResult Evaluate(ISentence argument)
        {
            throw new NotImplementedException();
        }
    }
}
