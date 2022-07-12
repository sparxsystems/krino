using Krino.Vertical.Utils.Rules;
using System.Diagnostics;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Rules
{
    [DebuggerDisplay("-{DebugView}")]
    public class WordEndsWithRule : RuleBase<IWord>, IRule<IWord>
    {
        private EndsWithStrRule myEndsWithStrRule;

        public WordEndsWithRule(string endOfWord)
        {
            myEndsWithStrRule = new EndsWithStrRule(endOfWord);
        }

        public string EndOfWord => myEndsWithStrRule.EndOfStr;

        public override bool Evaluate(IWord value) => myEndsWithStrRule.Evaluate(value.Value);

        public override bool Equals(IRule<IWord> other) => other is WordEndsWithRule rule && EndOfWord == rule.EndOfWord;

        public override int GetHashCode() => EndOfWord.GetHashCode();

        private string DebugView => EndOfWord;
    }
}
