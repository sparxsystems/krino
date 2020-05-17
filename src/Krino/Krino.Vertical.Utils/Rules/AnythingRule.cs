using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Accepts all values i.e. always returns TRUE.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("any")]
    public class AnythingRule<T> : IRule<T>
    {
        public bool Evaluate(T value) => true;

        public bool Equals(IRule<T> other) => other is AnythingRule<T>;
    }
}
