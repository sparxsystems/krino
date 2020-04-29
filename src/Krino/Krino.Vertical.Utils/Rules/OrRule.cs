using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Logical operator OR.
    /// </summary>
    [DebuggerDisplay("({mySubRule1} || {mySubRule2})")]
    public class OrRule<T> : IRule<T>
    {
        private IRule<T> mySubRule1;
        private IRule<T> mySubRule2;

        public OrRule(IRule<T> subRule1, IRule<T> subRule2)
        {
            mySubRule1 = subRule1;
            mySubRule2 = subRule2;
        }

        public bool Evaluate(T value) => mySubRule1.Evaluate(value) || mySubRule2.Evaluate(value);


        public bool Equals(IRule<T> other)
        {
            bool result = false;

            if (other is OrRule<T> orRule)
            {
                result = mySubRule1.Equals(orRule.mySubRule1) && mySubRule2.Equals(orRule.mySubRule2);
            }

            return result;
        }
    }
}
