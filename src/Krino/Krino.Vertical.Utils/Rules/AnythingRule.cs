namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Accepts all values i.e. always returns TRUE.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class AnythingRule<T> : IRule<T>
    {
        public bool Evaluate(T value) => true;
    }
}
