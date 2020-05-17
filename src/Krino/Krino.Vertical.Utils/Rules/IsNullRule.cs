namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Evaluates if the value is null.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class IsNullRule<T> : IRule<T>
        where T : class
    {
        public bool Evaluate(T value) => value == null;


        public bool Equals(IRule<T> other) => other is IsNullRule<T>;
    }
}
