using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes;

namespace Krino.Domain.ConstructiveAdpositionalArgumentation
{
    public class Argumentation
    {
        public bool IsArgument(ISentence sentence) => GrammarAttributes.Sentence.Complex.Argument.IsIn(sentence.Attributes);


        public Argument TryGetArgument(ISentence argumentSentence)
        {
            Argument result = null;

            if (IsArgument(argumentSentence))
            {
                result = new Argument(argumentSentence);
            }

            return result;
        }
    }
}
