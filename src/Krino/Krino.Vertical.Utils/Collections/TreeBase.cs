using Krino.Vertical.Utils.Diagnostic;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.Collections
{
    /// <summary>
    /// Abstract class to create the tree collection.
    /// </summary>
    /// <remarks>
    /// The tree collection can be created like a standard collection where the value is out of the collection logic.
    /// E.g. see the implementation of Tree which works like a standard .NET collection e.g. List.
    /// <br/>
    /// Or you can derive from TreeComposite and create a tree with custom properties and functions which arenot separated from the tree logic.
    /// </remarks>
    /// <typeparam name="C"></typeparam>
    public abstract class TreeBase<C> : ITreeBase<C>
        where C : class, ITreeBase<C>
    {
        private C myParent;
        private List<C> myChildren;
        private bool myInParentFlag;
        private bool myInAddFlag;
        private bool myInRemoveFlag;


        /// <summary>
        /// Returns the direct parent for this node.
        /// </summary>
        public C Parent
        {
            get => myParent;
            set
            {
                if (!myInParentFlag)
                {
                    try
                    {
                        myInParentFlag = true;

                        C aThis = (C)(object)this;

                        if (value != null)
                        {
                            value.Add(aThis);
                        }
                        else
                        {
                            myParent?.Remove(aThis);
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
        public IEnumerable<C> Parents
        {
            get
            {
                C parent = Parent;
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
        public IEnumerable<C> Siblings
        {
            get
            {
                if (Parent != null)
                {
                    foreach (C sibling in Parent.Children)
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
        public C Root { get { return Parent == null ? (C)(object)this : Parents.Last(); } }

        /// <summary>
        /// Returns leaves of this tree.
        /// </summary>
        public IEnumerable<C> Leaves { get { return this.Where(x => x.Children.Count == 0); } }


        /// <summary>
        /// Returns all direct children for this node.
        /// </summary>
        public IReadOnlyList<C> Children { get { return myChildren; } }


        /// <summary>
        /// Constructs the tree with children.
        /// </summary>
        /// <param name="children">direct children of this node.</param>
        public TreeBase(List<C> children = null)
        {
            myChildren = children ?? new List<C>();

            try
            {
                myInAddFlag = true;

                // Set parent for children.
                foreach (C aChild in myChildren)
                {
                    // Set this tree as the parent.
                    aChild.Parent = (C)(object)this;
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
        public void Add(C item)
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
                    item.Parent = (C)(object)this;
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
        public bool Remove(C item)
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
            //using var _t = Trace.Entering();

            // Recursivelly clear children.
            foreach (C aChild in myChildren)
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
        public bool Contains(C item)
        {
            // Check children and their children.
            foreach (C aChild in myChildren)
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
        public void CopyTo(C[] array, int arrayIndex)
        {
            int i = 0;
            foreach (C aTree in this)
            {
                array[arrayIndex + i] = aTree;
                ++i;
            }
        }


        /// <summary>
        /// Recursivelly counts number of children (not only direct but all).
        /// </summary>
        public int Count
        {
            get
            {
                int aCount = 0;
                foreach (C aTree in this)
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
        public IEnumerator<C> GetEnumerator()
        {
            Stack<C> aStack = new Stack<C>();
            aStack.Push((C)(object)this);

            while (aStack.Count > 0)
            {
                C aThis = aStack.Pop();
                yield return aThis;

                foreach (C aChild in aThis.Children.Reverse())
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
