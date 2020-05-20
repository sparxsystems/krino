using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Logical operator OR.
    /// </summary>
    [DebuggerDisplay("({Rule1} || {Rule2})")]
    public class OrRule<T> : IRule<T>
    {
        public OrRule(IRule<T> subRule1, IRule<T> subRule2)
        {
            Rule1 = subRule1;
            Rule2 = subRule2;
        }

        public IRule<T> Rule1 { get; private set; }
        public IRule<T> Rule2 { get; private set; }

        public bool Evaluate(T value) => Rule1.Evaluate(value) || Rule2.Evaluate(value);


        public bool Equals(IRule<T> other)
        {
            bool result = false;

            if (other is OrRule<T> orRule)
            {
                result = Rule1.Equals(orRule.Rule1) && Rule2.Equals(orRule.Rule2);
            }

            return result;
        }
    }
}
