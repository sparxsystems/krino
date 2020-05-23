namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Declares the binary operator rule e.g. and, or.
    /// </summary>
    public interface IBinaryOperatorRule<T> : IRule<T>
    {
        /// <summary>
        /// First rule.
        /// </summary>
        IRule<T> Subrule1 { get; }

        /// <summary>
        /// Second rule.
        /// </summary>
        IRule<T> Subrule2 { get; }
    }
}
