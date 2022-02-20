using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Domain.ConstructiveAdpositionalGrammar.Parsing.Rules;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.StateMachines;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public static class ParsingRule
    {
        private static ImmediateTriggerRule<IWord> myImmediateTriggerRule = new ImmediateTriggerRule<IWord>();

        public static IRule<IWord> WordStringIs(string word, bool caseSensitive = false) => new WordStringIsRule(word, caseSensitive);
        public static IRule<IWord> WordContainsAttribute(BigInteger attribute) => new WordAttributesRule(attribute);

        public static IRule<IWord> AuxiliaryWordIs(params string[] auxiliaryWords)
        {
            IRule<IWord> result = null;

            for (int i = 0; i < auxiliaryWords.Length; ++i)
            {
                var wordStr = auxiliaryWords[i];

                if (i == 0)
                {
                    result = WordStringIs(wordStr, false);
                }
                else
                {
                    result = result.Or(WordStringIs(wordStr, true));
                }
            }

            return result;
        }


        public static TransitionRule<LinguisticState, IWord> VerbPhraseContainsAttribute(BigInteger attribute)
            => new TransitionRule<LinguisticState, IWord>() { PathRule = new VerbPhraseAttributesRule(attribute), TriggerRule = ImmediateTrigger() };

        public static TransitionRule<LinguisticState, IWord> IsConcatenated() => new ItemIsConcatenatedTransitionRule();

        public static ImmediateTriggerRule<IWord> ImmediateTrigger() => myImmediateTriggerRule;
    }
}
