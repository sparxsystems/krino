using System.Collections.Generic;

namespace Krino.Vertical.Utils.Patterns
{
    /// <summary>
    /// Factory methods for particular rules.
    /// </summary>
    public static class It
    {
        public static IRule<T> Is<T>(T expectedValue, IEqualityComparer<T> comparer = null)
        {
            IRule<T> result = new Is<T>(expectedValue, comparer);
            return result;
        }

        public static IRule<T> IsAny<T>()
        {
            IRule<T> result = new IsAny<T>();
            return result;
        }
    }
}
