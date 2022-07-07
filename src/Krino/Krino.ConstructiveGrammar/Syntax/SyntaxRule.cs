using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.ConstructiveGrammar.Syntax.Rules;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.StateMachines;
using System.Numerics;

namespace Krino.ConstructiveGrammar.Syntax
{
    public static class SyntaxRule
    {
        private static ImmediateTriggerRule<IWord> myImmediateTriggerRule = new ImmediateTriggerRule<IWord>();

        public static IRule<IWord> WordIs(string word, bool caseSensitive = false) => new WordStringIsRule(word, caseSensitive);
        public static IRule<IWord> WordContainsAttribute(BigInteger attribute) => new WordAttributesRule(attribute);

        public static IRule<IWord> WordIsOneOf(params string[] auxiliaryWords)
        {
            IRule<IWord> result = null;

            for (int i = 0; i < auxiliaryWords.Length; ++i)
            {
                var wordStr = auxiliaryWords[i];

                if (i == 0)
                {
                    result = WordIs(wordStr, false);
                }
                else
                {
                    result = result.Or(WordIs(wordStr, true));
                }
            }

            return result;
        }

        public static IRule<IWord> WordIsNot(params string[] auxiliaryWords) => RuleMaker.Not(WordIsOneOf(auxiliaryWords));

        public static TransitionRule<LinguisticState, IWord> VerbPhraseContainsAttribute(BigInteger attribute)
            => new TransitionRule<LinguisticState, IWord>() { PathRule = new VerbPhraseAttributesRule(attribute), TriggerRule = ImmediateTrigger() };

        public static TransitionRule<LinguisticState, IWord> IsConcatenated() => new ItemIsConcatenatedTransitionRule();

        public static ImmediateTriggerRule<IWord> ImmediateTrigger() => myImmediateTriggerRule;
    }
}
