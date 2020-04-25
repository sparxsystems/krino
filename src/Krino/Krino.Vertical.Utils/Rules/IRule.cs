using System;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Declares the rule.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public interface IRule<T> : IEquatable<IRule<T>>
    {
        /// <summary>
        /// Evaluates the rule for the specifed value.
        /// </summary>
        /// <remarks>
        /// Returns true if the rule passed.
        /// </remarks>
        /// <param name="value"></param>
        /// <returns></returns>
        bool Evaluate(T value);
    }
}
