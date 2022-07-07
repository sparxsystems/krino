using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.LinguisticStructures.Attributes;

namespace Krino.ConstructiveArgumentation
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
