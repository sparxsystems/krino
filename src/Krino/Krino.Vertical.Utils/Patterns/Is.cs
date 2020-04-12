using System.Collections.Generic;

namespace Krino.Vertical.Utils.Patterns
{
    /// <summary>
    /// Defines the rule IsEqual.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class Is<T> : IRule<T>
    {
        private T myExpectedValue;
        private IEqualityComparer<T> myComparer;

        public Is(T expectedValue, IEqualityComparer<T> comparer = null)
        {
            myExpectedValue = expectedValue;

            if (comparer != null)
            {
                myComparer = comparer;
            }
            else if (expectedValue is IEqualityComparer<T>)
            {
                myComparer = (IEqualityComparer<T>)expectedValue;
            }
            else
            {
                myComparer = EqualityComparer<T>.Default;
            }
        }

        public bool Evaluate(T value)
        {
            bool result = myComparer.Equals(myExpectedValue, value);
            return result;
        }

        public bool Equals(IRule<T> other)
        {
            if (other is Is<T> r)
            {
                return myComparer.Equals(myExpectedValue, r.myExpectedValue);
            }

            return false;
        }
    }
}
