using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Logical operator AND.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("({Rule1} && {Rule2})")]
    public class AndRule<T> : IRule<T>
    {
        public AndRule(IRule<T> subRule1, IRule<T> subRule2)
        {
            Rule1 = subRule1;
            Rule2 = subRule2;
        }

        public IRule<T> Rule1 { get; private set; }
        public IRule<T> Rule2 { get; private set; }

        public bool Evaluate(T value) => Rule1.Evaluate(value) && Rule2.Evaluate(value);

        public bool Equals(IRule<T> other)
        {
            bool result = false;

            if (other is AndRule<T> andRule)
            {
                result = Rule1.Equals(andRule.Rule1) && Rule2.Equals(andRule.Rule2);
            }

            return result;
        }
    }
}
