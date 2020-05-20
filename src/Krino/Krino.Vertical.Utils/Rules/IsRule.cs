using System.Collections.Generic;
using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Simple rule evaluation.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("{RequiredValue}")]
    public class IsRule<T> : IRule<T>
    {
        private IEqualityComparer<T> myComparer;

        public IsRule(T requiredValue)
        {
            myComparer = EqualityComparer<T>.Default;
            RequiredValue = requiredValue;
        }

        public T RequiredValue { get; private set; }

        public bool Evaluate(T value) => RequiredValue.Equals(value);


        public bool Equals(IRule<T> other) => other is IsRule<T> isRule && myComparer.Equals(RequiredValue, isRule.RequiredValue);


        /// <summary>
        /// Implicitly converts the value into the rule.
        /// </summary>
        /// <param name="value"></param>
        public static implicit operator IsRule<T>(T value) => new IsRule<T>(value);
    }
}
