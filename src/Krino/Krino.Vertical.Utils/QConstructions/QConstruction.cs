using Krino.Vertical.Utils.Collections;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.QConstructions
{
    public class QConstruction<T> : IQConstruction<T>
    {
        private Func<T, T, bool> myCanConnect;

        public QConstruction(Func<T, T, bool> canConnectCallback)
        {
            myCanConnect = canConnectCallback;
        }

        public ITree<T> ActiveConstructions { get; private set; }

        public bool Add(T item)
        {
            bool isConnected = false;

            if (ActiveConstructions != null)
            {
                HashSet<ITree<T>> neededParts = new HashSet<ITree<T>>();

                foreach (ITree<T> leaf in ActiveConstructions.Leaves)
                {
                    if (myCanConnect(leaf.Value, item))
                    {
                        Tree<T> newItem = new Tree<T>(item);
                        leaf.Add(newItem);

                        neededParts.Add(newItem);
                        neededParts.AddRange(newItem.Parents);

                        isConnected = true;
                    }
                }

                if (isConnected)
                {
                    // Remove unused branches.
                    List<ITree<T>> toRemove = ActiveConstructions.Where(x => !neededParts.Contains(x)).ToList();
                    foreach (ITree<T> itemToRemove in toRemove)
                    {
                        // Disconnect the item from the tree.
                        itemToRemove.Parent = null;
                    }
                }
            }
            else
            {
                Tree<T> newItem = new Tree<T>(item);
                ActiveConstructions = newItem;
                isConnected = true;
            }

            return isConnected;
        }

        public void Clear()
        {
            ActiveConstructions.Clear();
        }
    }
}
