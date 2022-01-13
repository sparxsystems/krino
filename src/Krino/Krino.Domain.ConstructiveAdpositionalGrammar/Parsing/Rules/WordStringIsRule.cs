using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing.Rules
{
    [DebuggerDisplay("{DebugView}")]
    internal class WordStringIsRule : IRule<IWord>
    {
        private StringComparison myComparison;

        public WordStringIsRule(string word, bool isCaseSensitive = false)
        {
            myComparison = isCaseSensitive ? StringComparison.InvariantCulture : StringComparison.InvariantCultureIgnoreCase;
            WordStr = word;
        }

        public string WordStr { get; private set; }

        public bool Equals(IRule<IWord> other) => other is WordStringIsRule rule && WordStr.Equals(rule.WordStr, myComparison);

        public bool Evaluate(IWord value) => WordStr.Equals(value.Value, myComparison);

        private string DebugView => WordStr;
    }
}
