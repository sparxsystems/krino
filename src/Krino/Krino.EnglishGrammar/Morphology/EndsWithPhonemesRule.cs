using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Krino.EnglishGrammar.Morphology
{
    [DebuggerDisplay("{DebugView}")]
    public class EndsWithPhonemesRule : RuleBase<string>, IRule<string>
    {
        private Phoneme[] myPhonemes;

        public EndsWithPhonemesRule(params Phoneme[] phonemes)
        {
            myPhonemes = phonemes;
        }

        public IEnumerable<Phoneme> EndPhonemes => myPhonemes;

        public override bool Evaluate(string value)
        {
            bool result = value != null && myPhonemes.Length <= value.Length &&
                value.Reverse().Take(myPhonemes.Length).Select(x => x.GetPhoneme()).SequenceEqual(myPhonemes.Reverse());

            return result;
        }

        public override bool Equals(IRule<string> other) => other is EndsWithPhonemesRule rule && myPhonemes.SequenceEqual(rule.myPhonemes);

        public override int GetHashCode() => myPhonemes.CalculateHashCode();

        private string DebugView => string.Join(",", myPhonemes);
    }
}
