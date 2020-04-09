using System.Collections.Generic;

namespace Krino.Vertical.Utils.Collections
{
    /// <summary>
    /// Declares the common tree functionality.
    /// </summary>
    /// <typeparam name="C"></typeparam>
    public interface ITreeBase<C> : ICollection<C>
        where C : ITreeBase<C>
    {
        /// <summary>
        /// Distance from the root.
        /// </summary>
        int Generation { get; }

        /// <summary>
        /// Direct parent of this node. Null if it is the root.
        /// </summary>
        C Parent { get; set; }

        /// <summary>
        /// Parent sequnce to the root.
        /// </summary>
        IEnumerable<C> Parents { get; }

        /// <summary>
        /// Siblings of this node.
        /// </summary>
        IEnumerable<C> Siblings { get; }

        /// <summary>
        /// The root of this tree.
        /// </summary>
        /// <remarks>
        /// Returns this node if it is the root.
        /// </remarks>
        C Root { get; }

        /// <summary>
        /// Leaves of this tree.
        /// </summary>
        IEnumerable<C> Leaves { get; }

        /// <summary>
        /// Direct children of this node.
        /// </summary>
        IReadOnlyList<C> Children { get; }
    }
}
