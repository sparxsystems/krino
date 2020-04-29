using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Logical operator negation.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("!{mySubRule}")]
    public class NotRule<T> : IRule<T>
    {
        private IRule<T> mySubRule;

        public NotRule(IRule<T> subRule)
        {
            mySubRule = subRule;
        }

        public bool Evaluate(T value) => !mySubRule.Evaluate(value);

        public bool Equals(IRule<T> other) => other is NotRule<T> notRule && mySubRule.Equals(notRule.mySubRule);
    }
}
