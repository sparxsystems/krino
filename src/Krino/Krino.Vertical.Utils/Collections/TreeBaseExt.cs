using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.Collections
{
    /// <summary>
    /// Extension methods for the tree.
    /// </summary>
    public static class TreeBaseExt
    {
        /// <summary>
        /// Returns path to the root.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="tree"></param>
        /// <returns></returns>
        public static IEnumerable<T> GetPathToRoot<T>(this ITreeBase<T> tree)
            where T : class, ITreeBase<T> => tree.Parents.Prepend((T)tree);


        /// <summary>
        /// Restructures the tree so that the provided item is the new root of the tree.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="newRoot"></param>
        /// <returns></returns>
        public static T MakeRoot<T>(this T newRoot)
            where T : class, ITreeBase<T>
        {
            List<T> toRverseParents = new List<T>();
            toRverseParents.Add(newRoot);
            toRverseParents.AddRange(newRoot.Parents);

            for (int i = 0; i < toRverseParents.Count; ++i)
            {
                if (i == 0)
                {
                    toRverseParents[i].Parent = null;
                }
                else
                {
                    toRverseParents[i].Parent = toRverseParents[i - 1];
                }
            }

            return newRoot;
        }
    }
}
