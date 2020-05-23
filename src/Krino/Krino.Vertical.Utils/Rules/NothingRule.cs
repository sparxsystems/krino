using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Does not accept any value i.e. always returns false.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("nothing")]
    public class NothingRule<T> : RuleBase<T>, IRule<T>
    {
        public override bool Evaluate(T value) => false;

        public override bool Equals(IRule<T> other) => other is NothingRule<T>;
    }
}
