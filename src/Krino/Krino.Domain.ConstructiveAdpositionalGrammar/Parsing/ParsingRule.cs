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
        public static IRule<IWord> WordContainsAttribute(BigInteger attribute) => new WordContainsAttributeRule(attribute);

        public static IRule<StateTrace<LinguisticState, IWord>> PreviousWordContainsAttribute(BigInteger attribute) => new PreviousWordContainsAttributeRule(attribute);

        public static ImmediateTriggerRule<IWord> GetImmediateTrigger() => myImmediateTriggerRule;
    }
}
