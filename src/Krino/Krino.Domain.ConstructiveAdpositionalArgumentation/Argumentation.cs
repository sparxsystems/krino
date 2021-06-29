using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalArgumentation
{
    public class Argumentation : IArgumentation
    {
        public bool IsArgument(ISentence sentence) => StructureAttributes.Sentence.ComplexSentence.Argument.IsIn(sentence.Attributes);


        public ArgumentEvaluationResult Evaluate(ISentence argument)
        {
            throw new NotImplementedException();
        }
    }
}
