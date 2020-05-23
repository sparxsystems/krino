using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Logical operator AND.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("({Subrule1} && {Subrule2})")]
    public class AndRule<T> : RuleBase<T>, IBinaryOperatorRule<T>
    {
        public AndRule(IRule<T> subRule1, IRule<T> subRule2)
        {
            Subrule1 = subRule1;
            Subrule2 = subRule2;
        }

        public IRule<T> Subrule1 { get; private set; }
        public IRule<T> Subrule2 { get; private set; }

        public override bool Evaluate(T value) => Subrule1.Evaluate(value) && Subrule2.Evaluate(value);

        public override bool Equals(IRule<T> other)
        {
            bool result = false;

            if (other is AndRule<T> andRule)
            {
                result = Subrule1.Equals(andRule.Subrule1) && Subrule2.Equals(andRule.Subrule2);
            }

            return result;
        }
    }
}
