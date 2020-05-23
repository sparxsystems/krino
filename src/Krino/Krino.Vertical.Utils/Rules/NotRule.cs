using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Logical operator negation.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("!{Subrule}")]
    public class NotRule<T> : RuleBase<T>, IUnaryOperatorRule<T>
    {
        public NotRule(IRule<T> subRule)
        {
            Subrule = subRule;
        }

        public IRule<T> Subrule { get; private set; }

        public override bool Evaluate(T value) => !Subrule.Evaluate(value);

        public override bool Equals(IRule<T> other) => other is NotRule<T> notRule && Subrule.Equals(notRule.Subrule);
    }
}
