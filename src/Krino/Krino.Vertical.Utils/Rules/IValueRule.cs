namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Declares a rule which uses the reference value to evaluate the incoming value.
    /// </summary>
    public interface IValueRule<T> : IRule<T>
    {
        /// <summary>
        /// The refernce value.
        /// </summary>
        T Value { get; }
    }
}
