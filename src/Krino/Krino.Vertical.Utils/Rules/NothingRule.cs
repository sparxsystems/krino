using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Does not accept any value i.e. always returns false.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("false")]
    public class NothingRule<T> : IRule<T>
    {
        public bool Evaluate(T value) => false;

        public bool Equals(IRule<T> other) => other is NothingRule<T>;
    }
}
