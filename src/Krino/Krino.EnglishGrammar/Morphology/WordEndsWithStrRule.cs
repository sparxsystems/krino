using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Rules;
using System.Diagnostics;

namespace Krino.EnglishGrammar.Morphology
{
    [DebuggerDisplay("{DebugView}")]
    public class WordEndsWithStrRule : RuleBase<IWord>, IRule<IWord>
    {
        private EndsWithStrRule myEndsWithStrRule;

        public WordEndsWithStrRule(string endOfStr)
        {
            myEndsWithStrRule = new EndsWithStrRule(endOfStr);
        }

        public string EndOfStr => myEndsWithStrRule.EndOfStr;

        public override bool Evaluate(IWord value) => myEndsWithStrRule.Evaluate(value?.Value);


        public override bool Equals(IRule<IWord> other) => other is WordEndsWithStrRule rule && myEndsWithStrRule.Equals(rule.myEndsWithStrRule);

        public override int GetHashCode() => myEndsWithStrRule.GetHashCode();

        private string DebugView => EndOfStr;
    }
}
