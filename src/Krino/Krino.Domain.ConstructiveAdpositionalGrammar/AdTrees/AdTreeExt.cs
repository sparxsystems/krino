using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
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
        public static IAdTree MakeDeepCopy(this IAdTree adTree)
        {
            var root = adTree;

            // <original, copy>
            Stack<Tuple<IAdTree, IAdTree>> stack = new Stack<Tuple<IAdTree, IAdTree>>();

            var rootMorpheme = new Morpheme(root.Morpheme);
            var rootPattern = new Pattern(root.Pattern);
            IAdTree rootCopy = new AdTree(rootMorpheme, rootPattern);
            stack.Push(Tuple.Create(root, rootCopy));

            while (stack.Count > 0)
            {
                Tuple<IAdTree, IAdTree> aThis = stack.Pop();

                if (aThis.Item1.Left != null)
                {
                    var morpheme = new Morpheme(aThis.Item1.Left.Morpheme);
                    var pattern = new Pattern(aThis.Item1.Left.Pattern);
                    IAdTree leftCopy = new AdTree(morpheme, pattern);
                    aThis.Item2.Left = leftCopy;
                    stack.Push(Tuple.Create(aThis.Item1.Left, leftCopy));
                }

                if (aThis.Item1.Right != null)
                {
                    var morpheme = new Morpheme(aThis.Item1.Right.Morpheme);
                    var pattern = new Pattern(aThis.Item1.Right.Pattern);
                    IAdTree rightCopy = new AdTree(morpheme, pattern);
                    aThis.Item2.Right = rightCopy;
                    stack.Push(Tuple.Create(aThis.Item1.Right, rightCopy));
                }
            }

            return rootCopy;
        }

        /// <summary>
        /// Returns the shallow copy (Morphemes and Patterns are not duplicated) of the adtree. The returned copy is on the same path.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static IAdTree MakeShallowCopy(this IAdTree adTree)
        {
            IAdTree root = adTree.Root;

            // Store the current position in the tree.
            AdTreePosition[] path = root != adTree ? adTree.GetPath() : null;

            // <original, copy>
            Stack<Tuple<IAdTree, IAdTree>> stack = new Stack<Tuple<IAdTree, IAdTree>>();
            IAdTree rootCopy = new AdTree(root.Morpheme, root.Pattern);
            stack.Push(Tuple.Create(root, rootCopy));

            while (stack.Count > 0)
            {
                Tuple<IAdTree, IAdTree> aThis = stack.Pop();

                if (aThis.Item1.Left != null)
                {
                    IAdTree leftCopy = new AdTree(aThis.Item1.Left.Morpheme, aThis.Item1.Left.Pattern);
                    aThis.Item2.Left = leftCopy;
                    lock (stack)
                    {
                        stack.Push(Tuple.Create(aThis.Item1.Left, leftCopy));
                    }
                }

                if (aThis.Item1.Right != null)
                {
                    IAdTree rightCopy = new AdTree(aThis.Item1.Right.Morpheme, aThis.Item1.Right.Pattern);
                    aThis.Item2.Right = rightCopy;
                    lock (stack)
                    {
                        stack.Push(Tuple.Create(aThis.Item1.Right, rightCopy));
                    }
                }
            }

            if (path != null)
            {
                // Return the tree element in the copy which is on the same path as the input parameter.
                if (rootCopy.TryGetAdTree(path, out IAdTree result))
                {
                    return result;
                }
            }
            else
            {
                return rootCopy;
            }
            

            throw new InvalidOperationException("Failed to properly copy the adtree.");
        }

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
