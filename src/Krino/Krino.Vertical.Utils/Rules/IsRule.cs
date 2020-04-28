using System;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Simple rule evaluation.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class IsRule<T> : IRule<T>
        where T : IEquatable<T>
    {
        private T myRequiredValue;

        public IsRule(T requiredValue)
        {
            myRequiredValue = requiredValue;
        }

        public bool Evaluate(T value) => myRequiredValue.Equals(value);

        /// <summary>
        /// Implicitly converts the value into the rule.
        /// </summary>
        /// <param name="value"></param>
        public static implicit operator IsRule<T>(T value) => new IsRule<T>(value);
    }
}
