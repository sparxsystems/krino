using System.Collections.Generic;
using System.Diagnostics;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Simple rule evaluation.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("{myRequiredValue}")]
    public class IsRule<T> : IRule<T>
    {
        private IEqualityComparer<T> myComparer;
        private T myRequiredValue;

        public IsRule(T requiredValue)
        {
            myComparer = EqualityComparer<T>.Default;
            myRequiredValue = requiredValue;
        }

        public bool Evaluate(T value) => myRequiredValue.Equals(value);


        public bool Equals(IRule<T> other) => other is IsRule<T> isRule && myComparer.Equals(myRequiredValue, isRule.myRequiredValue);


        /// <summary>
        /// Implicitly converts the value into the rule.
        /// </summary>
        /// <param name="value"></param>
        public static implicit operator IsRule<T>(T value) => new IsRule<T>(value);
    }
}
