using System.Collections.Generic;
using System.Diagnostics;

namespace Krino.Vertical.Utils.Collections
{
    /// <summary>
    /// Generic tree collection.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("{Value.ToString()}")]
    public class Tree<T> : TreeBase<ITree<T>>, ITree<T>
    {
        public Tree(T value, List<ITree<T>> children = null)
            : base(children)
        {
            Value = value;
        }

        public T Value { get; set; }
    }

}
