using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Rules;
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
        /// Returns sequence from the specified adtree to the root.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static IEnumerable<IAdTree> GetSequenceToRoot(this IAdTree adTree) => new IAdTree[] { adTree }.Concat(adTree.AdPositions);

        /// <summary>
        /// Returns the path to the adTree element. Empty array if it is the root.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static byte[] GetPath(this IAdTree adTree)
        {
            IEnumerable<IAdTree> adTreesOnPath = adTree.GetSequenceToRoot().Where(x => x.AdPosition != null);
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
        /// <param name="adTreeElementToRight"></param>
        /// <returns></returns>
        public static bool CanAttachToRight(this IAdTree adTree, IAdTree adTreeElementToRight)
        {
            // If the rule allows to attach something to the right.
            if (!adTree.Pattern.RightRule.Equals(PatternRule.Nothing))
            {
                // If the valency position is specified then check correctness with regard to presence of previous valencies.
                if (adTree.Pattern.ValencyPosition > 0)
                {
                    IAdTree closestValencyAdPosition = new IAdTree[] { adTreeElementToRight }.Concat(adTreeElementToRight.RightChildren)
                        .FirstOrDefault(x => x.Pattern.ValencyPosition > 0);

                    if (closestValencyAdPosition == null && adTree.Pattern.ValencyPosition > 1 ||
                        closestValencyAdPosition != null && adTree.Pattern.ValencyPosition != closestValencyAdPosition.Pattern.ValencyPosition + 1)
                    {
                        return false;
                    }
                }

                // If the right rule of the adtree matches the element.
                if (adTree.Pattern.RightRule.IsMatch(adTreeElementToRight.Morpheme.Morph, adTreeElementToRight.Morpheme.Attributes, adTreeElementToRight.Pattern.PatternAttributes) ||
                    // or if the right rule of the adtree matches the right rule of the element - inheritance.
                    adTreeElementToRight.Pattern.RightRule.IsSubruleOf(adTree.Pattern.RightRule))
                {
                    return true;
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
                    // or if the LEFT rule of the adtree matches the RIGHT rule of the element - inheritance works always via the right child.
                    adTreeElement.Pattern.RightRule.IsSubruleOf(adTree.Pattern.LeftRule))
                {
                    return true;
                }
            }

            return false;
        }


        /// <summary>
        /// Gets the first adtree element which is attached on the left or null if it does not exist.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static IAdTree GetFirstAdPositionOnLeft(this IAdTree adTree) => adTree.GetSequenceToRoot().FirstOrDefault(x => x.IsOnLeft);


        /// <summary>
        /// Inserts the adtree into the adtree.
        /// </summary>
        /// <param name="adTree">The adtree into which it shall be inserted.</param>
        /// <param name="toInsert">The adtree which shall be inserted.</param>
        /// <param name="whereReattach">The adtree which shall reatach the adtree disconnected by the insertion.</param>
        /// <param name="reattachPosition">The position where the adtree shall be reattached after the insertion.</param>
        public static void Insert(this IAdTree adTree, IAdTree toInsert, IAdTree whereReattach, AttachPosition reattachPosition)
        {
            if (adTree.AdPosition != null)
            {
                // Insert the toInsert instead of the adTree.
                if (adTree.IsOnLeft)
                {
                    adTree.AdPosition.Left = toInsert;
                }
                else if (adTree.IsOnRight)
                {
                    adTree.AdPosition.Right = toInsert;
                }
            }

            // Attach the adtree to the inserted adtree.
            whereReattach.Attach(adTree, reattachPosition);
        }


        /// <summary>
        /// Appends the adtree to the adtree.
        /// </summary>
        /// <param name="adTree">The adtree into whihc it shall be appended.</param>
        /// <param name="toAppend">The adtree which shall be appended.</param>
        /// <param name="appendPosition">The position how it shall be appended.</param>
        public static void Attach(this IAdTree adTree, IAdTree toAppend, AttachPosition appendPosition)
        {
            if (appendPosition == AttachPosition.ChildOnLeft)
            {
                adTree.Left = toAppend;
            }
            else if (appendPosition == AttachPosition.ChildOnRight)
            {
                adTree.Right = toAppend;
            }
        }


        /// <summary>
        /// Removes the adtree from the adtree structure.
        /// </summary>
        /// <param name="adTree">The adtree which shall be removed.</param>
        public static void Detach(this IAdTree adTree)
        {
            if (adTree.AdPosition != null)
            {
                if (adTree.IsOnLeft)
                {
                    adTree.AdPosition.Left = null;
                }
                else if (adTree.IsOnRight)
                {
                    adTree.AdPosition.Right = null;
                }
            }
        }


        private static bool IsSubruleOf(this PatternRule patternRule, PatternRule other)
        {
            if (other.MorphemeRule.GrammarCharacter == GrammarCharacter.Epsilon ||
                other.MorphemeRule.GrammarCharacter == patternRule.MorphemeRule.GrammarCharacter)
            {
                if (other.MorphemeRule.MorphRule.Equals(MorphRuleMaker.Anything) ||
                    other.MorphemeRule.MorphRule.Equals(patternRule.MorphemeRule.MorphRule))
                {
                    if (other.MorphemeRule.AttributesRule.Equals(MaskRule.Anything) ||
                        other.MorphemeRule.AttributesRule.Equals(patternRule.MorphemeRule.AttributesRule))
                    {
                        if (other.PatternAttributesRule.Equals(MaskRule.Anything) ||
                            other.PatternAttributesRule.Equals(patternRule.PatternAttributesRule))

                        return true;
                    }
                }
            }

            return false;
        }
    }
}
