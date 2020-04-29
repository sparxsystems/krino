using System;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Declares the rule builder.
    /// </summary>
    /// <typeparam name="T">type of the value which shall be evaluated.</typeparam>
    public interface IRule<T> : IEquatable<IRule<T>>
    {
        /// <summary>
        /// Evaluates the rule.
        /// </summary>
        /// <remarks>
        /// Returns true if the value complies the rule.
        /// </remarks>
        /// <param name="value"></param>
        /// <returns></returns>
        bool Evaluate(T value);
    }
}
