using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    [DebuggerDisplay("{DebugView}")]
    public class EndsWithStrRule : RuleBase<string>, IRule<string>
    {
        public EndsWithStrRule(string endOfStr)
        {
            EndOfStr = endOfStr;
        }

        public string EndOfStr { get; private set; }

        public override bool Evaluate(string value) => value != null && value.EndsWith(EndOfStr);

        public override bool Equals(IRule<string> other) => other is EndsWithStrRule rule && EndOfStr == rule.EndOfStr;

        public override int GetHashCode() => EndOfStr.GetHashCode();

        private string DebugView => EndOfStr;
    }
}
