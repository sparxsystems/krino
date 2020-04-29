using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.PatternAttributesArrangement;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    /// <summary>
    /// Extension helper methods for the adtree.
    /// </summary>
    public static class AdTreeExt
    {
        /// <summary>
        /// Returns the path to the adTree element. Empty array if it is the root.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static byte[] GetPath(this IAdTree adTree)
        {
            IEnumerable<IAdTree> adTreesOnPath = new IAdTree[] { adTree }.Concat(adTree.AdPositions).Where(x => x.AdPosition != null);
            byte[] result = adTreesOnPath.Select(x => x.IsOnLeft ? (byte)1 : (byte)2).Reverse().ToArray();
            return result;
        }

        /// <summary>
        /// Tries to get the adtree element which is on the specified path.
        /// </summary>
        /// <param name="rootAdTree"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static bool TryGetAdTree(this IAdTree rootAdTree, byte[] path, out IAdTree result)
        {
            result = rootAdTree;
            foreach (byte value in path)
            {
                if (value == 1)
                {
                    if (result.Left == null)
                    {
                        return false;
                    }

                    result = result.Left;
                }
                else if (value == 2)
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
        /// Returns true if the adTreeElement can be attached to the right branch of the adTree.
        /// </summary>
        /// <param name="adTree"></param>
        /// <param name="adTreeElement"></param>
        /// <returns></returns>
        public static bool CanAttachToRight(this IAdTree adTree, IAdTree adTreeElement)
        {
            // If the rule allows to attach something to the right.
            if (!adTree.Pattern.RightRule.Equals(PatternRule.Nothing))
            {
                // If the valency position is specified.
                if (adTree.Pattern.ValencyPosition > 1)
                {
                    // Check if previous valency is present in the element which shall be attached.
                    if (adTree.Pattern.ValencyPosition - 1 != adTreeElement.Pattern.ValencyPosition &&
                        !adTreeElement.RightChildren.Any(x => adTree.Pattern.ValencyPosition - 1 == x.Pattern.ValencyPosition))
                    {
                        return false;
                    }
                }

                // If the right rule of the adtree matches the element.
                if (adTree.Pattern.RightRule.IsMatch(adTreeElement.Morpheme.Morph, adTreeElement.Morpheme.Attributes, adTreeElement.Pattern.PatternAttributes) ||
                    // or if the right rule of the adtree matches the right rule of the element - inheritance.
                    adTree.Pattern.RightRule.Equals(adTreeElement.Pattern.RightRule))
                {
                    // If the adposition rule matches the adtree.
                    if (adTreeElement.Pattern.AdPositionRule.IsMatch(adTree.Morpheme.Morph, adTree.Morpheme.Attributes, adTree.Pattern.PatternAttributes))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        /// <summary>
        /// Returns true if the adTreeElement can be attached to the left branch of the adTree.
        /// </summary>
        /// <param name="adTree"></param>
        /// <param name="adTreeElement"></param>
        /// <returns></returns>
        public static bool CanAttachToLeft(this IAdTree adTree, IAdTree adTreeElement)
        {
            // If the rule allows to attach something to the left.
            if (!adTree.Pattern.LeftRule.Equals(PatternRule.Nothing))
            {
                // If the left rule of the adtree matches the element.
                if (adTree.Pattern.LeftRule.IsMatch(adTreeElement.Morpheme.Morph, adTreeElement.Morpheme.Attributes, adTreeElement.Pattern.PatternAttributes) ||
                    // or if the LEFT rule of the adtree matches the RIGHT rule of the element - inheritance works always via the right branch.
                    adTree.Pattern.LeftRule.Equals(adTreeElement.Pattern.RightRule))
                {
                    // If there is a match from the adtree element to the adtree via the adposition.
                    if (adTreeElement.Pattern.AdPositionRule.IsMatch(adTree.Morpheme.Morph, adTree.Morpheme.Attributes, adTree.Pattern.PatternAttributes))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

    }
}
