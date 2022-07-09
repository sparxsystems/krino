using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.Vertical.Utils.StateMachines;
using System.Numerics;

namespace Krino.ConstructiveGrammar.Syntax.Rules
{
    public static class SyntaxRules
    {
        private static ImmediateTriggerRule<IWord> myImmediateTriggerRule = new ImmediateTriggerRule<IWord>();

        public static TransitionRule<LinguisticState, IWord> VerbPhraseContainsAttribute(BigInteger attribute)
            => new TransitionRule<LinguisticState, IWord>() { PathRule = new VerbPhraseAttributesRule(attribute), TriggerRule = ImmediateTrigger() };

        public static TransitionRule<LinguisticState, IWord> IsConcatenated() => new ItemIsConcatenatedTransitionRule();

        public static ImmediateTriggerRule<IWord> ImmediateTrigger() => myImmediateTriggerRule;
    }
}
