namespace Krino.Vertical.Utils.Patterns
{
    /// <summary>
    /// Implements the rule the value can be whatever.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class IsAny<T> : IRule<T>
    {
        public bool Evaluate(T value)
        {
            return true;
        }

        public bool Equals(IRule<T> other)
        {
            if (other is IsAny<T>)
            {
                return true;
            }

            return false;
        }
    }
}
