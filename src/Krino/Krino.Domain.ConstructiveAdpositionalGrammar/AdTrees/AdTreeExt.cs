using Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Collections;
using Krino.Vertical.Utils.Rules;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    /// <summary>
    /// Extension helper methods for the adtree.
    /// </summary>
    public static class AdTreeExt
    {
        /// <summary>
        /// Returns the signature (sequence of grammar characters) for the sequence of individual adtree items.
        /// </summary>
        /// <remarks>
        /// AdTree items in the sequence shall not be within the same adtree.
        /// </remarks>
        /// <param name="adTreeSequence"></param>
        /// <returns></returns>
        public static string GetSignature(this IEnumerable<IAdTree> adTreeSequence)
        {
            var result = new StringBuilder();

            foreach (var item in adTreeSequence)
            {
                var itemSignature = GetSignature(item);
                result.Append(itemSignature);
            }

            return result.ToString();
        }

        /// <summary>
        /// Returns the signature (sequence of grammar characters) for the adtree.
        /// </summary>
        /// <param name="adTreeSequence"></param>
        /// <returns></returns>
        public static string GetSignature(this IAdTree adTree)
        {
            var result = new StringBuilder();

            foreach (var item in adTree)
            {
                if (item.Morpheme.GrammarCharacter != GrammarCharacter.e && !string.IsNullOrEmpty(item.Morpheme.Morph))
                {
                    result.Append(item.Morpheme.GrammarCharacter);
                }
            }

            return result.ToString();
        }

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
        /// Returns the shallow copy (Morphemes and Patterns are not duplicated) of the adtree. The returned copy is on the same path.
        /// </summary>
        /// <param name="adTree"></param>
        /// <returns></returns>
        public static IAdTree MakeShallowCopy(this IAdTree adTree)
        {
            // Store the current position in the tree.
            AttachingPosition[] path = adTree.GetPath();

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
        public static AttachingPosition[] GetPath(this IAdTree adTree)
        {
            IEnumerable<IAdTree> adTreesOnPath = adTree.GetSequenceToRoot().Where(x => x.AdPosition != null);
            AttachingPosition[] result = adTreesOnPath.Select(x => x.IsOnLeft ? AttachingPosition.ChildOnLeft : AttachingPosition.ChildOnRight).Reverse().ToArray();
            return result;
        }

        /// <summary>
        /// Tries to get the adtree element which is on the specified path.
        /// </summary>
        /// <param name="rootAdTree"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static bool TryGetAdTree(this IAdTree rootAdTree, AttachingPosition[] path, out IAdTree result)
        {
            result = rootAdTree;
            foreach (AttachingPosition value in path)
            {
                if (value == AttachingPosition.ChildOnLeft)
                {
                    if (result.Left == null)
                    {
                        return false;
                    }

                    result = result.Left;
                }
                else if (value == AttachingPosition.ChildOnRight)
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
        /// <param name="attributesModel"></param>
        /// <returns></returns>
        public static bool CanAttachToRight(this IAdTree adTree, IAdTree adTreeToAttach, IAttributesModel attributesModel)
        {
            bool result = adTree.CanAttachViaRule(adTreeToAttach, AttachingPosition.ChildOnRight, attributesModel);
            return result;
        }

        /// <summary>
        /// Returns true if the adTreeElement can be attached to the left branch of the adTree.
        /// </summary>
        /// <param name="adTree"></param>
        /// <param name="adTreeToAttach"></param>
        /// <param name="attributesModel"></param>
        /// <returns></returns>
        public static bool CanAttachToLeft(this IAdTree adTree, IAdTree adTreeToAttach, IAttributesModel attributesModel)
        {
            bool result = adTree.CanAttachViaRule(adTreeToAttach, AttachingPosition.ChildOnLeft, attributesModel);
            return result;
        }

        private static bool CanAttachViaRule(this IAdTree adTree, IAdTree adTreeToAttach, AttachingPosition attachPosition, IAttributesModel attributesModel)
        {
            // Get rule to evalute.
            MorphemeRule rule = attachPosition == AttachingPosition.ChildOnLeft ? adTree.Pattern.LeftRule : adTree.Pattern.RightRule;

            // If the rule allows to attach and the order of attaching is correct.
            if (!rule.Equals(MorphemeRule.Nothing) && IsAttachingOrderCorrect(adTree, attachPosition))
            {
                // If the adtree where to attach is morphematic then it is not possible to attach the second child until the morpheme is set.
                if (adTree.Pattern.IsMorphematicAdPosition() && (adTree.Right != null || adTree.Left != null) && string.IsNullOrEmpty(adTree.Morpheme.Morph))
                {
                    return false;
                }

                // Note: from the tree to attach we need to get the adtree representing the morpheme which shall be evaluated.
                IAdTree morphemeAdTree = null;

                // If the adTreeToAttach is a morpheme or a transference.
                if (adTreeToAttach.Pattern.IsLikeMorpheme)
                {
                    morphemeAdTree = adTreeToAttach;
                }
                // It is an adposition.
                else if (adTreeToAttach.Pattern.IsEpsilonAdPosition() ||
                         adTreeToAttach.Pattern.IsMorphematicAdPosition())
                {
                    // If the rule allows the substitution.
                    if (rule.SubstitutionRule.Evaluate(adTreeToAttach.Morpheme.GrammarCharacter))
                    {
                        // If it shall be attached to the right and
                        // the valency position is specified then check correctness with regard to presence of previously filled valencies.
                        if (attachPosition == AttachingPosition.ChildOnRight)
                        {
                            IAdTree valencyElement = adTree.GetSequenceToRoot()
                                .TakeUntil(x => x.IsOnRight)
                                .FirstOrDefault(x => x.Pattern.ValencyPosition > 0);

                            if (valencyElement != null)
                            {
                                IAdTree previousValencyElement = adTreeToAttach.GetRightSequence()
                                    .FirstOrDefault(x => x.Pattern.ValencyPosition > 0);

                                if (previousValencyElement == null && valencyElement.Pattern.ValencyPosition > 1 ||
                                    previousValencyElement != null && valencyElement.Pattern.ValencyPosition != previousValencyElement.Pattern.ValencyPosition + 1)
                                {
                                    return false;
                                }
                            }
                        }

                        // Get the governor from the right children.
                        morphemeAdTree = adTreeToAttach.RightChildren.FirstOrDefault(x => x.IsGovernor);

                        // If the governor is not attached yet then check only rules.
                        if (morphemeAdTree == null)
                        {
                            IEnumerable<IAdTree> rightSequence = new IAdTree[] { adTreeToAttach }.Concat(adTreeToAttach.RightChildren);
                            if (rightSequence.Any(x => !x.Pattern.RightRule.IsSubruleOf(rule) && !rule.IsSubruleOf(x.Pattern.RightRule)))
                            {
                                return false;
                            }

                            return true;
                        }
                    }
                }

                if (morphemeAdTree != null)
                {
                    // If it shall be attached to the right then check the valency.
                    if (attachPosition == AttachingPosition.ChildOnRight && attributesModel.IsVerb(morphemeAdTree.Morpheme.Attributes))
                    {
                        int valency = attributesModel.GetNumberOfValencies(morphemeAdTree.Morpheme.Attributes);

                        if (valency > -1)
                        {
                            // Get already filled valency positions.
                            int[] valencyPositions = morphemeAdTree.GetSequenceToRoot()
                                .TakeWhile(x => x != adTree) // This is to not make an assuption if it is already attached or not.
                                .TakeUntil(x => x.IsOnRight)
                                .Concat(adTree.GetSequenceToRoot().TakeUntil(x => x.IsOnRight))
                                .Where(x => x.Pattern.ValencyPosition > 0)
                                .Select(x => x.Pattern.ValencyPosition)
                                .ToArray();

                            // If such valency is already filled.
                            if (valencyPositions.Length > valency)
                            {
                                return false;
                            }
                            for (int i = 0; i < valencyPositions.Length; ++i)
                            {
                                if (valencyPositions[i] != i + 1)
                                {
                                    return false;
                                }
                            }
                        }
                    }

                    Morpheme morphemeToEvaluate;

                    if (morphemeAdTree.Pattern.IsMonoTransference)
                    {
                        morphemeToEvaluate = new Morpheme(attributesModel, morphemeAdTree.Right.Morpheme.Morph, morphemeAdTree.Morpheme.Attributes);
                    }
                    else if (morphemeAdTree.Pattern.IsPairTransference)
                    {
                        string relevantMorph = morphemeAdTree.RightChildren.FirstOrDefault(x => !string.IsNullOrEmpty(x.Morpheme.Morph))?.Morpheme.Morph;
                        if (relevantMorph != null)
                        {
                            morphemeToEvaluate = new Morpheme(attributesModel, relevantMorph, morphemeAdTree.Morpheme.Attributes);
                        }
                        else
                        {
                            // The morpheme with the morph is not attached yet so check only attributes.
                            if (rule.AttributesRule.Evaluate(morphemeAdTree.Morpheme.Attributes))
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
                        morphemeToEvaluate = morphemeAdTree.Morpheme;
                    }

                    // Check if the morpheme passes the rule.
                    if (attachPosition == AttachingPosition.ChildOnLeft)
                    {
                        bool result = rule.Evaluate(morphemeToEvaluate);
                        return result;
                    }
                    else
                    {
                        // Check if the morpheme passes the rule in all relevant adpositions.
                        bool result = true;
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

                        return result;
                    }
                }
            }

            return false;
        }

        private static bool IsAttachingOrderCorrect(IAdTree adTree, AttachingPosition position)
        {
            bool result = false;

            // If the attaching position is on the left.
            if (position == AttachingPosition.ChildOnLeft)
            {
                // If left is before the right
                if (adTree.Pattern.IsLeftFirst ||
                    // or right is already set
                    adTree.Right != null ||
                    // or right is not supposed to be set.
                    adTree.Pattern.RightRule.Equals(MorphemeRule.Nothing))
                {
                    result = true;
                }
            }
            else
            {
                if (!adTree.Pattern.IsLeftFirst ||
                    adTree.Left != null ||
                    adTree.Pattern.LeftRule.Equals(MorphemeRule.Nothing))
                {
                    result = true;
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
        public static void Insert(this IAdTree adTree, IAdTree toInsert, IAdTree whereReattach, AttachingPosition reattachPosition)
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
        public static void Attach(this IAdTree adTree, IAdTree toAttach, AttachingPosition appendPosition)
        {
            if (appendPosition == AttachingPosition.ChildOnLeft)
            {
                adTree.Left = toAttach;
            }
            else if (appendPosition == AttachingPosition.ChildOnRight)
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
        public static IEnumerable<IAdTree> GetNonconformities(this IAdTree adTree, IAttributesModel attributesModel)
        {
            foreach (IAdTree item in adTree)
            {
                if (!item.IsCorrect(attributesModel))
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
        public static bool IsCorrect(this IAdTree adTree, IAttributesModel attributesModel)
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
                if (!adTree.CanAttachToLeft(adTree.Left, attributesModel))
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
                if (!adTree.CanAttachToRight(adTree.Right, attributesModel))
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

        /// <summary>
        /// Returns true if the adtree has filled all required data.
        /// </summary>
        /// <remarks>
        /// It does not evaluate rules. It just checks if required data are present.
        /// </remarks>
        /// <param name="adtree"></param>
        /// <returns></returns>
        public static bool IsComplete(this IAdTree adTree)
        {
            // Check if the morph is set.
            if (string.IsNullOrEmpty(adTree.Morpheme.Morph) &&
                !adTree.Pattern.MorphemeRule.MorphRule.Equals(MorphRules.Nothing) &&
                !adTree.Pattern.MorphemeRule.MorphRule.Evaluate(adTree.Morpheme.Morph))
            {
                return false;
            }

            // Check if the attribute is set.
            if (adTree.Morpheme.Attributes == 0 &&
                !adTree.Pattern.MorphemeRule.AttributesRule.Equals(MaskRule.Nothing) &&
                !adTree.Pattern.MorphemeRule.AttributesRule.Equals(MaskRule.Anything))
            {
                return false;
            }


            // Left
            if (adTree.Left == null &&
                !adTree.Pattern.LeftRule.Equals(MorphemeRule.Nothing) &&
                !adTree.Pattern.LeftRule.Equals(MorphemeRule.Anything))
            {
                return false;
            }


            // Right
            if (adTree.Right == null &&
                !adTree.Pattern.RightRule.Equals(MorphemeRule.Nothing) &&
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
                if (other.MorphRule.Equals(MorphRules.Anything) ||
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
