using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Logical operator AND.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("({mySubRule1} && {mySubRule2})")]
    public class AndRule<T> : IRule<T>
    {
        private IRule<T> mySubRule1;
        private IRule<T> mySubRule2;

        public AndRule(IRule<T> subRule1, IRule<T> subRule2)
        {
            mySubRule1 = subRule1;
            mySubRule2 = subRule2;
        }

        public bool Evaluate(T value) => mySubRule1.Evaluate(value) && mySubRule2.Evaluate(value);

        public bool Equals(IRule<T> other)
        {
            bool result = false;

            if (other is AndRule<T> andRule)
            {
                result = mySubRule1.Equals(andRule.mySubRule1) && mySubRule2.Equals(andRule.mySubRule2);
            }

            return result;
        }
    }
}
