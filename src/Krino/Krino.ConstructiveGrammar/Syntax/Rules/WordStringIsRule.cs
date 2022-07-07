using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;

namespace Krino.ConstructiveGrammar.Syntax.Rules
{
    [DebuggerDisplay("{DebugView}")]
    internal class WordStringIsRule : RuleBase<IWord>, IRule<IWord>
    {
        private StringComparison myComparison;

        public WordStringIsRule(string word, bool isCaseSensitive = false)
        {
            myComparison = isCaseSensitive ? StringComparison.InvariantCulture : StringComparison.InvariantCultureIgnoreCase;
            WordStr = word;
        }

        public string WordStr { get; private set; }

        public override bool Evaluate(IWord value) => WordStr.Equals(value.Value, myComparison);

        public override bool Equals(IRule<IWord> other) => other is WordStringIsRule rule && WordStr.Equals(rule.WordStr, myComparison);

        private string DebugView => WordStr;
    }
}
