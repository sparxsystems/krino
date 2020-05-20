using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Logical operator negation.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("!{NegatedRule}")]
    public class NotRule<T> : IRule<T>
    {
        public NotRule(IRule<T> subRule)
        {
            NegatedRule = subRule;
        }

        public IRule<T> NegatedRule { get; private set; }

        public bool Evaluate(T value) => !NegatedRule.Evaluate(value);

        public bool Equals(IRule<T> other) => other is NotRule<T> notRule && NegatedRule.Equals(notRule.NegatedRule);
    }
}
