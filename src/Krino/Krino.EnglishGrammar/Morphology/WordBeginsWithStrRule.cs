using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Rules;
using System.Diagnostics;

namespace Krino.EnglishGrammar.Morphology
{
    [DebuggerDisplay("{DebugView}")]
    public class WordBeginsWithStrRule : RuleBase<IWord>, IRule<IWord>
    {
        private BeginsWithStrRule myBeginWithStrRule;

        public WordBeginsWithStrRule(string beginningOfStr)
        {
            myBeginWithStrRule = new BeginsWithStrRule(beginningOfStr);
        }

        public string BeginningOfStr => myBeginWithStrRule.BeginningOfStr;

        public override bool Evaluate(IWord value) => myBeginWithStrRule.Evaluate(value?.Value);


        public override bool Equals(IRule<IWord> other) => other is WordBeginsWithStrRule rule && myBeginWithStrRule.Equals(rule.myBeginWithStrRule);

        public override int GetHashCode() => myBeginWithStrRule.GetHashCode();

        private string DebugView => BeginningOfStr;
    }
}
