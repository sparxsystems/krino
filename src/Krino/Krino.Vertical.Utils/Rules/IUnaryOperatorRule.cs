namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Declares the unary operator rule.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public interface IUnaryOperatorRule<T> : IRule<T>
    {
        /// <summary>
        /// Subrule of the unary operator.
        /// </summary>
        IRule<T> Subrule { get; }
    }
}
