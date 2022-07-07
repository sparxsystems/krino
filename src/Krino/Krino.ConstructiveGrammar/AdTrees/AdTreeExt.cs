using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.ConstructiveGrammar.AdTrees
{
    /// <summary>
    /// Extension helper methods for the adtree.
    /// </summary>
    public static class AdTreeExt
    {
        /// <summary>
        /// Returns the sequence from the specified adtree to the root.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static IEnumerable<IAdTree> GetSequenceToRoot(this IAdTree adTree) => new IAdTree[] { adTree }.Concat(adTree.AdPositions);

        /// <summary>
        /// Returns the sequence from the specified adtree through right children.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static IEnumerable<IAdTree> GetRightSequence(this IAdTree adTree) => new IAdTree[] { adTree }.Concat(adTree.RightChildren);

        /// <summary>
        /// Returns the deep copy of the sub-adTree. (adTree will be the root of the returned adTree)
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        
        /// <summary>
        /// Returns the path to the adTree element. Empty array if it is the root.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static AdTreePosition[] GetPath(this IAdTree adTree)
        {
            IEnumerable<IAdTree> adTreesOnPath = adTree.GetSequenceToRoot().Where(x => x.AdPosition != null);
            AdTreePosition[] result = adTreesOnPath.Select(x => x.IsOnLeft ? AdTreePosition.ChildOnLeft : AdTreePosition.ChildOnRight).Reverse().ToArray();
            return result;
        }

        /// <summary>
        /// Tries to get the adtree element which is on the specified path.
        /// </summary>
        /// <param name="rootAdTree"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static bool TryGetAdTree(this IAdTree rootAdTree, AdTreePosition[] path, out IAdTree result)
        {
            result = rootAdTree;
            foreach (AdTreePosition value in path)
            {
                if (value == AdTreePosition.ChildOnLeft)
                {
                    if (result.Left == null)
                    {
                        return false;
                    }

                    result = result.Left;
                }
                else if (value == AdTreePosition.ChildOnRight)
                {
                    if (result.Right == null)
                    {
                        return false;
                    }

                    result = result.Right;
                }
                else
                {
                    throw new InvalidOperationException($"Failed to get the adTree element because the path ontained the incorrect value {value}. Expected is 1 or 2.");
                }
            }

            return true;
        }

        /// <summary>
        /// Gets the first adtree element which is attached on the left or null if it does not exist.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static IAdTree GetFirstAdPositionOnLeft(this IAdTree adTree) => adTree.GetSequenceToRoot().FirstOrDefault(x => x.IsOnLeft);

    }
}
