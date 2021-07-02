using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Logical operator OR.
    /// </summary>
    [DebuggerDisplay("({Subrule1} || {Subrule2})")]
    public class OrRule<T> : RuleBase<T>, IBinaryOperatorRule<T>
    {
        public OrRule(IRule<T> subRule1, IRule<T> subRule2)
        {
            Subrule1 = subRule1;
            Subrule2 = subRule2;
        }

        public IRule<T> Subrule1 { get; private set; }
        public IRule<T> Subrule2 { get; private set; }

        public override bool Evaluate(T value) => Subrule1.Evaluate(value) || Subrule2.Evaluate(value);


        public override bool Equals(IRule<T> other)
        {
            bool result = false;

            if (other is OrRule<T> orRule)
            {
                result = Subrule1.Equals(orRule.Subrule1) && Subrule2.Equals(orRule.Subrule2);
            }

            return result;
        }

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ Subrule1.GetHashCode();
            hash = (hash * 16777619) ^ Subrule2.GetHashCode();

            return hash;
        }
    }
}
