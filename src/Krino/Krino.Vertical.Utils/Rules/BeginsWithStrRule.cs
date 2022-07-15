using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    [DebuggerDisplay("{DebugView}")]
    public class BeginsWithStrRule : RuleBase<string>, IRule<string>
    {
        public BeginsWithStrRule(string beginningOfStr)
        {
            BeginningOfStr = beginningOfStr;
        }

        public string BeginningOfStr { get; private set; }

        public override bool Evaluate(string value) => value != null && value.StartsWith(BeginningOfStr);

        public override bool Equals(IRule<string> other) => other is BeginsWithStrRule rule && BeginningOfStr == rule.BeginningOfStr;

        public override int GetHashCode() => BeginningOfStr.GetHashCode();

        private string DebugView => BeginningOfStr;
    }
}
