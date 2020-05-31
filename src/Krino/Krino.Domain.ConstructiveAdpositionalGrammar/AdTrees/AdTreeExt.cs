using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
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
        /// Returns the shallow copy (Morpheme and Pattern are not duplicated) of the adtree. The returned copy is on the same path.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static IAdTree MakeShallowCopy(this IAdTree adTree)
        {
            // Store the current position in the tree.
            AttachPosition[] path = adTree.GetPath();

            IAdTree root = adTree.Root;

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
                    stack.Push(Tuple.Create(aThis.Item1.Left, leftCopy));
                }

                if (aThis.Item1.Right != null)
                {
                    IAdTree rightCopy = new AdTree(aThis.Item1.Right.Morpheme, aThis.Item1.Right.Pattern);
                    aThis.Item2.Right = rightCopy;
                    stack.Push(Tuple.Create(aThis.Item1.Right, rightCopy));
                }
            }

            // Return the tree element in the copy which is on the same path as the input parameter.
            if (rootCopy.TryGetAdTree(path, out IAdTree result))
            {
                return result;
            }

            throw new InvalidOperationException("Failed to properly copy the adtree.");
        }

        /// <summary>
        /// Returns the path to the adTree element. Empty array if it is the root.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static AttachPosition[] GetPath(this IAdTree adTree)
        {
            IEnumerable<IAdTree> adTreesOnPath = adTree.GetSequenceToRoot().Where(x => x.AdPosition != null);
            AttachPosition[] result = adTreesOnPath.Select(x => x.IsOnLeft ? AttachPosition.ChildOnLeft : AttachPosition.ChildOnRight).Reverse().ToArray();
            return result;
        }

        /// <summary>
        /// Tries to get the adtree element which is on the specified path.
        /// </summary>
        /// <param name="rootAdTree"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static bool TryGetAdTree(this IAdTree rootAdTree, AttachPosition[] path, out IAdTree result)
        {
            result = rootAdTree;
            foreach (AttachPosition value in path)
            {
                if (value == AttachPosition.ChildOnLeft)
                {
                    if (result.Left == null)
                    {
                        return false;
                    }

                    result = result.Left;
                }
                else if (value == AttachPosition.ChildOnRight)
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
        /// <param name="adTreeToAttach"></param>
        /// <returns></returns>
        public static bool CanAttachToRight(this IAdTree adTree, IAdTree adTreeToAttach)
        {
            // If the rule allows to attach something to the right.
            if (!adTree.Pattern.RightRule.Equals(MorphemeRule.Nothing))
            {
                // If the right child shall be attached after the left child
                // and the left child is not attached yet.
                if (adTree.Pattern.RightRule.Order > adTree.Pattern.LeftRule.Order &&
                    !adTree.Pattern.LeftRule.Equals(MorphemeRule.Nothing) &&
                    adTree.Left == null)
                {
                    return false;
                }

                // If the valency position is specified then check correctness with regard to presence of previous valencies.
                if (adTree.Pattern.MorphemeRule.ValencyPosition > 0)
                {
                    IAdTree closestValencyAdPosition = new IAdTree[] { adTreeToAttach }.Concat(adTreeToAttach.RightChildren)
                        .FirstOrDefault(x => x.Pattern.MorphemeRule.ValencyPosition > 0);

                    if (closestValencyAdPosition == null && adTree.Pattern.MorphemeRule.ValencyPosition > 1 ||
                        closestValencyAdPosition != null && adTree.Pattern.MorphemeRule.ValencyPosition != closestValencyAdPosition.Pattern.MorphemeRule.ValencyPosition + 1)
                    {
                        return false;
                    }
                }

                if (adTree.CanAttachViaRule(false, adTreeToAttach))
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
        /// <param name="adTreeToAttach"></param>
        /// <returns></returns>
        public static bool CanAttachToLeft(this IAdTree adTree, IAdTree adTreeToAttach)
        {
            // If the rule allows to attach something to the left.
            if (!adTree.Pattern.LeftRule.Equals(MorphemeRule.Nothing))
            {
                // If the right child shall be attached first but is not attached yet.
                if (adTree.Pattern.LeftRule.Order > adTree.Pattern.RightRule.Order &&
                    !adTree.Pattern.RightRule.Equals(MorphemeRule.Nothing) &&
                    adTree.Right == null)
                {
                    return false;
                }

                if (adTree.CanAttachViaRule(true, adTreeToAttach))
                {
                    return true;
                }
            }

            return false;
        }

        private static bool CanAttachViaRule(this IAdTree adTree, bool attachToLeft , IAdTree adTreeToAttach)
        {
            bool result = false;

            MorphemeRule rule = attachToLeft ? adTree.Pattern.LeftRule : adTree.Pattern.RightRule;

            IAdTree morphematicAdTree = null;

            // If the adtree to attach is epsilon or U adposition.
            if (adTreeToAttach.Morpheme.GrammarCharacter == GrammarCharacter.e ||
                adTreeToAttach.Morpheme.GrammarCharacter == GrammarCharacter.U)
            {
                // If the rule allows the inheritance.
                if (rule.InheritanceRule.Evaluate(adTreeToAttach.Morpheme.GrammarCharacter))
                {
                    // Get the first adtree on the right which is not epsilon or U adposition.
                    morphematicAdTree = adTreeToAttach.RightChildren.FirstOrDefault(x =>
                        x.Morpheme.GrammarCharacter != GrammarCharacter.e &&
                        x.Morpheme.GrammarCharacter != GrammarCharacter.U);

                    // If such adtree does not exist it means it was not attached yet so check only rules.
                    if (morphematicAdTree == null)
                    {
                        IEnumerable<IAdTree> rightSequence = new IAdTree[] { adTreeToAttach }.Concat(adTreeToAttach.RightChildren);
                        if (rightSequence.All(x => x.Pattern.RightRule.IsSubruleOf(rule) || rule.IsSubruleOf(x.Pattern.RightRule)))
                        {
                            return true;
                        }
                        else
                        {
                            return false;
                        }
                    }
                }
            }
            else
            {
                morphematicAdTree = adTreeToAttach;
            }

            if (morphematicAdTree != null)
            {
                Morpheme morphemeToEvaluate;

                if (morphematicAdTree.Pattern.IsPrimitiveTransference())
                {
                    morphemeToEvaluate = new Morpheme(morphematicAdTree.Right.Morpheme.Morph, morphematicAdTree.Morpheme.Attributes);
                }
                else if (morphematicAdTree.Pattern.IsTransference())
                {
                    string relevantMorph = morphematicAdTree.RightChildren.FirstOrDefault(x => !string.IsNullOrEmpty(x.Morpheme.Morph))?.Morpheme.Morph;
                    if (relevantMorph != null)
                    {
                        morphemeToEvaluate = new Morpheme(relevantMorph, morphematicAdTree.Morpheme.Attributes);
                    }
                    else
                    {
                        // The morpheme with the morph is not attached yet so check only attributes.
                        if (rule.AttributesRule.Evaluate(morphematicAdTree.Morpheme.Attributes))
                        {
                            return true;
                        }
                        else
                        {
                            return false;
                        }
                    }
                }
                else
                {
                    morphemeToEvaluate = morphematicAdTree.Morpheme;
                }

                // Check if the morpheme passes the rule.
                if (attachToLeft)
                {
                    result = rule.Evaluate(morphemeToEvaluate);
                }
                else
                {
                    // Check if the morpheme passes the rule in all relevant adpositions.
                    result = true;
                    IEnumerable<IAdTree> sequenceToRoot = adTree.GetSequenceToRoot();
                    foreach (IAdTree item in sequenceToRoot)
                    {
                        if (!item.Pattern.RightRule.Evaluate(morphemeToEvaluate))
                        {
                            result = false;
                            break;
                        }

                        if (item.IsOnLeft)
                        {
                            if (item.AdPosition.Morpheme.GrammarCharacter != GrammarCharacter.e &&
                                item.AdPosition.Morpheme.GrammarCharacter != GrammarCharacter.U &&
                                !item.AdPosition.Pattern.LeftRule.Evaluate(morphemeToEvaluate))
                            {
                                result = false;
                            }

                            break;
                        }

                        if (item.Morpheme.GrammarCharacter != GrammarCharacter.e &&
                            item.Morpheme.GrammarCharacter != GrammarCharacter.U)
                        {
                            break;
                        }
                    }
                }
            }

            return result;
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
            // Insert the toInsert instead of the adTree.
            if (adTree.IsOnLeft)
            {
                adTree.AdPosition.Left = toInsert;
            }
            else if (adTree.IsOnRight)
            {
                adTree.AdPosition.Right = toInsert;
            }

            // Attach the adtree to the inserted adtree.
            whereReattach.Attach(adTree, reattachPosition);
        }


        /// <summary>
        /// Replaces the adtree with the 'replace' adtree.
        /// </summary>
        /// <param name="adTree">AdTree which shall be replaced.</param>
        /// <param name="replace">AdTree which shall be used insted of the original one.</param>
        public static void Replace(this IAdTree adTree, IAdTree replace)
        {
            if (adTree.IsOnLeft)
            {
                adTree.AdPosition.Left = replace;
            }
            else if (adTree.IsOnRight)
            {
                adTree.AdPosition.Right = replace;
            }

            replace.Left = adTree.Left;
            replace.Right = adTree.Right;
        }


        /// <summary>
        /// Appends the adtree to the adtree.
        /// </summary>
        /// <param name="adTree">The adtree into which it shall be attached.</param>
        /// <param name="toAttach">The adtree which shall be appended.</param>
        /// <param name="appendPosition">The position how it shall be appended.</param>
        public static void Attach(this IAdTree adTree, IAdTree toAttach, AttachPosition appendPosition)
        {
            if (appendPosition == AttachPosition.ChildOnLeft)
            {
                adTree.Left = toAttach;
            }
            else if (appendPosition == AttachPosition.ChildOnRight)
            {
                adTree.Right = toAttach;
            }
        }

        /// <summary>
        /// Removes the adtree from the adtree structure.
        /// </summary>
        /// <param name="adTree">The adtree which shall be removed.</param>
        public static void Detach(this IAdTree adTree)
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

        /// <summary>
        /// Returns all adtrees which do not match the pattern.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static IEnumerable<IAdTree> GetNonconformities(this IAdTree adTree)
        {
            foreach (IAdTree item in adTree)
            {
                if (!item.Evaluate())
                {
                    yield return item;
                }
            }
        }

        /// <summary>
        /// Evaluates if the adtree matches its pattern.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static bool Evaluate(this IAdTree adTree)
        {
            // Check the morpheme belonging to the adtree.
            if (!adTree.Pattern.MorphemeRule.Evaluate(adTree.Morpheme))
            {
                if (!adTree.Pattern.MorphemeRule.Equals(MorphemeRule.Nothing) ||
                    (adTree.Pattern.MorphemeRule.Equals(MorphemeRule.Nothing) &&
                     !string.IsNullOrEmpty(adTree.Morpheme.Morph)))
                {
                    return false;
                }
            }

            // Left
            if (adTree.Left != null)
            {
                if (!adTree.CanAttachToLeft(adTree.Left))
                {
                    return false;
                }
            }
            else if (!adTree.Pattern.LeftRule.Equals(MorphemeRule.Nothing) &&
                     !adTree.Pattern.LeftRule.Equals(MorphemeRule.Anything))
            {
                return false;
            }


            // Right
            if (adTree.Right != null)
            {
                if (!adTree.CanAttachToRight(adTree.Right))
                {
                    return false;
                }
            }
            else if (!adTree.Pattern.RightRule.Equals(MorphemeRule.Nothing) &&
                     !adTree.Pattern.RightRule.Equals(MorphemeRule.Anything))
            {
                return false;
            }

            return true;
        }

        

        private static bool IsSubruleOf(this MorphemeRule morphemeRule, MorphemeRule other)
        {
            if (other.GrammarCharacter == GrammarCharacter.e ||
                other.GrammarCharacter == morphemeRule.GrammarCharacter)
            {
                if (other.MorphRule.Equals(MorphRuleMaker.Anything) ||
                    morphemeRule.MorphRule.IsSubruleOf(other.MorphRule))
                {
                    if (other.AttributesRule.Equals(MaskRule.Anything) ||
                        morphemeRule.AttributesRule.IsSubruleOf(other.AttributesRule))
                    {
                        return true;
                    }
                }
            }

            return false;
        }
    }
}
