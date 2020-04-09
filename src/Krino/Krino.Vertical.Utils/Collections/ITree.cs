namespace Krino.Vertical.Utils.Collections
{
    /// <summary>
    /// Declares the tree collection.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public interface ITree<T> : ITreeBase<ITree<T>>
    {
        /// <summary>
        /// The value of this node.
        /// </summary>
        T Value { get; set; }
    }
}
