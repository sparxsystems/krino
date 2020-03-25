using System.Linq;
using System.Collections.Generic;
using System.Diagnostics;

namespace Krino.Vertical.Utils.Collections
{
    /// <summary>
    /// Generic tree collection.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    [DebuggerDisplay("{Value.ToString()}")]
    public class Tree<T> : ITree<T>
    {
        private ITree<T> myParent;
        private List<ITree<T>> myChildren;
        private bool myInParentFlag;
        private bool myInAddFlag;
        private bool myInRemoveFlag;


        /// <summary>
        /// Gets or sets the value of this node. 
        /// </summary>
        public T Value { get; set; }

        /// <summary>
        /// Returns the direct parent for this node.
        /// </summary>
        public ITree<T> Parent
        {
            get => myParent;
            set
            {
                if (!myInParentFlag)
                {
                    try
                    {
                        myInParentFlag = true;
                        
                        if (value != null)
                        {
                            value.Add(this);
                        }
                        else
                        {
                            myParent?.Remove(this);
                        }

                        myParent = value;
                    }
                    finally
                    {
                        myInParentFlag = false;
                    }
                }
            }
        }

        /// <summary>
        /// Returns the number specifying how far is this node from the root.
        /// </summary>
        public int Generation { get { return Parents.Count(); } }

        /// <summary>
        /// Returns all parents to the root.
        /// </summary>
        /// <remarks>
        /// The root item is the last one in the sequence.
        /// </remarks>
        public IEnumerable<ITree<T>> Parents
        {
            get
            {
                ITree<T> parent = Parent;
                while (parent != null)
                {
                    yield return parent;

                    parent = parent.Parent;
                }
            }
        }

        /// <summary>
        /// Returns all siblings to this node. 
        /// </summary>
        public IEnumerable<ITree<T>> Siblings
        {
            get
            {
                if (Parent != null)
                {
                    foreach (Tree<T> sibling in Parent.Children)
                    {
                        if (sibling != this)
                        {
                            yield return sibling;
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Returns the root of the tree.
        /// </summary>
        public ITree<T> Root { get { return Parent == null ? this : Parents.Last(); } }

        /// <summary>
        /// Returns leaves of this tree.
        /// </summary>
        public IEnumerable<ITree<T>> Leaves { get { return this.Where(x => x.Children.Count == 0); } }
        

        /// <summary>
        /// Returns all direct children for this node.
        /// </summary>
        public IReadOnlyList<ITree<T>> Children { get { return myChildren; } }


        /// <summary>
        /// Constructs the tree with children.
        /// </summary>
        /// <param name="value">value of this node.</param>
        /// <param name="children">direct children of this node.</param>
        public Tree(T value, List<ITree<T>> children = null)
        {
            Value = value;
            myChildren = children ?? new List<ITree<T>>();

            try
            {
                myInAddFlag = true;

                // Set parent for children.
                foreach (Tree<T> aChild in myChildren)
                {
                    // Set this tree as the parent.
                    aChild.Parent = this;
                }
            }
            finally
            {
                myInAddFlag = false;
            }
        }

        #region ICollection<T>

        /// <summary>
        /// Adds the child to direct children of this node.
        /// </summary>
        /// <param name="item">child node to be added</param>
        public void Add(ITree<T> item)
        {
            if (!myInAddFlag)
            {
                try
                {
                    myInAddFlag = true;

                    if (item.Parent != null)
                    {
                        item.Parent.Remove(item);
                    }

                    myChildren.Add(item);
                    item.Parent = this;
                }
                finally
                {
                    myInAddFlag = false;
                }
            }
        }

        /// <summary>
        /// Removes the child from direct children of this node.
        /// </summary>
        /// <param name="item">tree item to be removed.</param>
        /// <returns>returns true if the item was found and deleted.</returns>
        public bool Remove(ITree<T> item)
        {
            if (!myInRemoveFlag)
            {
                try
                {
                    myInRemoveFlag = true;

                    if (myChildren.Remove(item))
                    {
                        item.Parent = null;
                        return true;
                    }
                }
                finally
                {
                    myInRemoveFlag = false;
                }
            }

            return false;
        }

        /// <summary>
        /// Recursivelly clears all children.
        /// </summary>
        public void Clear()
        {
            // Recursivelly clear children.
            foreach (ITree<T> aChild in myChildren)
            {
                aChild.Parent = null;
                aChild.Clear();
            }

            // Clear container of this branch.
            myChildren.Clear();
        }

        /// <summary>
        /// Returns true if the item is present.
        /// </summary>
        /// <param name="item"></param>
        /// <returns></returns>
        public bool Contains(ITree<T> item)
        {
            // Check children and their children.
            foreach (ITree<T> aChild in myChildren)
            {
                if (aChild.Contains(item))
                {
                    return true;
                }
            }

            return myChildren.Contains(item);
        }

        /// <summary>
        /// Copies all childs (not only direct but all) to the array.
        /// </summary>
        /// <param name="array"></param>
        /// <param name="arrayIndex"></param>
        public void CopyTo(ITree<T>[] array, int arrayIndex)
        {
            int i = 0;
            foreach (Tree<T> aTree in this)
            {
                array[arrayIndex + i] = aTree;
                ++i;
            }
        }


        /// <summary>
        /// Recurivelly counts number of children (not only direct but all).
        /// </summary>
        public int Count
        {
            get
            {
                int aCount = 0;
                foreach (ITree<T> aTree in this)
                {
                    ++aCount;
                }

                return aCount;
            }
        }

        /// <summary>
        /// Returns false.
        /// </summary>
        public bool IsReadOnly => false;
        

        

        #endregion //ICollection<T>


        #region IEnumerable<T>

        /// <summary>
        /// Returns the enumerator to iterate through all children (not only direct but all).
        /// </summary>
        /// <returns></returns>
        public IEnumerator<ITree<T>> GetEnumerator()
        {
            Stack<ITree<T>> aStack = new Stack<ITree<T>>();
            aStack.Push(this);

            while (aStack.Count > 0)
            {
                ITree<T> aThis = aStack.Pop();
                yield return aThis;

                foreach (ITree<T> aChild in aThis.Children.Reverse())
                {
                    aStack.Push(aChild);
                }
            }
        }


        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion //IEnumerable<T>
    }

}
