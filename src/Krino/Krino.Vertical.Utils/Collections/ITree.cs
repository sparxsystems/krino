using System.Collections.Generic;

namespace Krino.Vertical.Utils.Collections
{
    /// <summary>
    /// Declares the tree.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public interface ITree<T> : ICollection<ITree<T>>
    {
        /// <summary>
        /// The value of this node.
        /// </summary>
        T Value { get; set; }

        /// <summary>
        /// Distance from the root.
        /// </summary>
        int Generation { get; }

        /// <summary>
        /// Direct parent of this node. Null if it is the root.
        /// </summary>
        ITree<T> Parent { get; set; }

        /// <summary>
        /// Parent sequnce to the root.
        /// </summary>
        IEnumerable<ITree<T>> Parents { get; }

        /// <summary>
        /// Siblings of this node.
        /// </summary>
        IEnumerable<ITree<T>> Siblings { get; }

        /// <summary>
        /// The root of this tree.
        /// </summary>
        /// <remarks>
        /// Returns this node if it is the root.
        /// </remarks>
        ITree<T> Root { get; }

        /// <summary>
        /// Leaves of this tree.
        /// </summary>
        IEnumerable<ITree<T>> Leaves { get; }

        /// <summary>
        /// Direct children of this node.
        /// </summary>
        IReadOnlyList<ITree<T>> Children { get; }
    }
}
