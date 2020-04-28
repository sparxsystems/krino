namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Does not accept any value i.e. always returns false.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class NothingRule<T> : IRule<T>
    {
        public bool Evaluate(T value) => false;
    }
}
