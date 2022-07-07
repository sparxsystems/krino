using Krino.Domain.ConstructiveGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes;

namespace Krino.Domain.ConstructiveArgumentation
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
