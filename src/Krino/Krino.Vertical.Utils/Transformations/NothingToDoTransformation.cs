namespace Krino.Vertical.Utils.Transformations
{
    /// <summary>
    /// This transformation makes nothing.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class NothingToDoTransformation<T> : ITransformation<T>
    {
        /// <summary>
        /// Returns the original value.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public T Transform(T value) => value;

        public bool Equals(ITransformation<T> other) => other is NothingToDoTransformation<T>;
    }
}
