using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace Krino.EnglishGrammar.Morphology
{
    [DebuggerDisplay("{DebugView}")]
    public class BeginsWithPhonemesRule : RuleBase<string>, IRule<string>
    {
        private Phoneme[] myPhonemes;

        public BeginsWithPhonemesRule(params Phoneme[] phonemes)
        {
            myPhonemes = phonemes;
        }

        public IEnumerable<Phoneme> BeginPhonemes => myPhonemes;

        public override bool Evaluate(string value)
        {
            using var _ = Krino.Vertical.Utils.Diagnostic.Trace.Entering();

            bool result = value != null && myPhonemes.Length <= value.Length &&
                value.Take(myPhonemes.Length).Select(x => x.GetPhoneme()).SequenceEqual(myPhonemes);

            return result;
        }

        public override bool Equals(IRule<string> other) => other is BeginsWithPhonemesRule rule && myPhonemes.SequenceEqual(rule.myPhonemes);

        public override int GetHashCode() => myPhonemes.CalculateHashCode();

        private string DebugView => string.Join(",", myPhonemes);
    }
}
