using Krino.Vertical.Utils.Rules;
using System.Diagnostics;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Rules
{
    [DebuggerDisplay("-{DebugView}")]
    public class WordEndsWithRule : RuleBase<IWord>, IRule<IWord>
    {
        public WordEndsWithRule(string endOfWord)
        {
            EndOfWord = endOfWord;
        }

        public string EndOfWord { get; private set; }

        public override bool Evaluate(IWord value) => value.Value.EndsWith(EndOfWord);

        public override bool Equals(IRule<IWord> other) => other is WordEndsWithRule rule && EndOfWord == rule.EndOfWord;

        public override int GetHashCode() => EndOfWord.GetHashCode();

        private string DebugView => EndOfWord;
    }
}
