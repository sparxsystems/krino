using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Rules;
using System.Collections.Generic;
using System.Diagnostics;

namespace Krino.EnglishGrammar.Morphology
{
    [DebuggerDisplay("{DebugView}")]
    public class WordBeginsWithPhonemesRule : RuleBase<IWord>, IRule<IWord>
    {
        private BeginsWithPhonemesRule myBeginWithPhonemesRule;

        public WordBeginsWithPhonemesRule(params Phoneme[] phonemes)
        {
            myBeginWithPhonemesRule = new BeginsWithPhonemesRule(phonemes);
        }

        public IEnumerable<Phoneme> BeginPhonemes => myBeginWithPhonemesRule.BeginPhonemes;

        public override bool Evaluate(IWord value) => myBeginWithPhonemesRule.Evaluate(value?.Value);
        

        public override bool Equals(IRule<IWord> other) => other is WordBeginsWithPhonemesRule rule && myBeginWithPhonemesRule.Equals(rule.myBeginWithPhonemesRule);

        public override int GetHashCode() => myBeginWithPhonemesRule.GetHashCode();

        private string DebugView => string.Join(",", myBeginWithPhonemesRule.BeginPhonemes);
    }
}
