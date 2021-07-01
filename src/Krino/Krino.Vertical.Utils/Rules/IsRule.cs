using System.Collections.Generic;
using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Simple rule evaluation.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("{DebugView}")]
    public class IsRule<T> : RuleBase<T>, IValueRule<T>
    {
        private IEqualityComparer<T> myComparer;

        public IsRule(T requiredValue)
        {
            myComparer = EqualityComparer<T>.Default;
            Value = requiredValue;
        }

        /// <summary>
        /// The value which is accepted by the rule.
        /// </summary>
        public T Value { get; private set; }

        /// <summary>
        /// Returns true if the value is equal to the reference value.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public override bool Evaluate(T value) => Value.Equals(value);


        public override bool Equals(IRule<T> other) => other is IsRule<T> isRule && myComparer.Equals(Value, isRule.Value);


        /// <summary>
        /// Implicitly converts the value into the rule.
        /// </summary>
        /// <param name="value"></param>
        public static implicit operator IsRule<T>(T value) => new IsRule<T>(value);


        private string DebugView => Value.ToString();
    }
}
