using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Evaluates if the value is null.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("null")]
    public class IsNullRule<T> : RuleBase<T>, IReferenceValueRule<T>
        where T : class
    {
        public T ReferenceValue => null;

        /// <summary>
        /// Returns true if the value is null.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public override bool Evaluate(T value) => value == null;

        public override bool Equals(IRule<T> other) => other is IsNullRule<T>;
    }
}
