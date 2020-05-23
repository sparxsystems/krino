using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Accepts all values i.e. always returns TRUE.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("any")]
    public class AnythingRule<T> : RuleBase<T>, IRule<T>
    {
        public override bool Evaluate(T value) => true;

        public override bool Equals(IRule<T> other) => other is AnythingRule<T>;
    }
}
