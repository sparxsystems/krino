﻿using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class AdTreeBuilder
    {
        private IEnumerable<IPattern> myPatterns;
        private List<IAdTree> myActiveAdTrees = new List<IAdTree>();

        public AdTreeBuilder(IEnumerable<IPattern> patterns)
        {
            myPatterns = patterns;
        }

        public IReadOnlyList<IAdTree> ActiveAdTrees => myActiveAdTrees.Select(x => x.Root).ToList();

        public bool AddMorpheme(IMorpheme morpheme)
        {
            bool isAdded = false;

            IEnumerable<IPattern> morphemeMatchingPatterns = GetApplicablePatterns(morpheme);

            // Update active adtrees.
            if (myActiveAdTrees.Count != 0)
            {
                List<IAdTree> newListOfAdTrees = new List<IAdTree>();

                // Go via active adtrees (i.e. adtrees which are still candidates to become the final adtree).
                foreach(IAdTree currentAdTree in myActiveAdTrees)
                {
                    // Go via patterns which match the incoming morpheme.
                    foreach (IPattern pattern in morphemeMatchingPatterns)
                    {
                        IAdTree newAdTreeElement = new AdTree()
                        {
                            Morpheme = morpheme,
                            Pattern = pattern,
                        };

                        AttachAdTreeElement(currentAdTree, newAdTreeElement, newListOfAdTrees);
                    }
                }

                if (newListOfAdTrees.Count > 0)
                {
                    myActiveAdTrees = newListOfAdTrees;
                    isAdded = true;
                }
            }
            // Create new adtrees.
            else
            {
                foreach (IPattern pattern in morphemeMatchingPatterns)
                {
                    AdTree newAdTree = new AdTree()
                    {
                        Morpheme = morpheme,
                        Pattern = pattern
                    };

                    myActiveAdTrees.Add(newAdTree);
                }

                if (myActiveAdTrees.Count > 0)
                {
                    isAdded = true;
                }
            }


            return isAdded;
        }


        private IEnumerable<IPattern> GetApplicablePatterns(IMorpheme morpheme)
        {
            foreach (IPattern pattern in myPatterns)
            {
                if (pattern.MorphemeRule.IsMatch(morpheme.Morph, morpheme.Attributes))
                {
                    yield return pattern;
                }
            }
        }

        private void AttachAdTreeElement(IAdTree adTree, IAdTree newAdTreeElement, List<IAdTree> adTrees)
        {
            // Try to use the adtree as the adposition and attach the new element to its left.
            if (adTree.Left == null && adTree.CanAttachToLeft(newAdTreeElement))
            {
                IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                adTreeCopy.Left = newAdTreeElementCopy;

                IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                adTrees.Add(workInProgress);
            }

            // Try to use the adtree as the adposition and attach the new element to its right.
            if (adTree.Right == null && adTree.CanAttachToRight(newAdTreeElement))
            {
                IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                adTreeCopy.Right = newAdTreeElementCopy;

                IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                adTrees.Add(workInProgress);
            }


            // If the adtree adposition is still free.
            if (adTree.AdPosition == null)
            {
                // Try to use the new element as the adposition and attach the adtree to its left.
                if (newAdTreeElement.CanAttachToLeft(adTree))
                {
                    IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                    IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                    newAdTreeElementCopy.Left = adTreeCopy;

                    IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                    adTrees.Add(workInProgress);
                }

                // Try to use the new element as the adposition and attach the adtree to its right.
                if (newAdTreeElement.CanAttachToRight(adTree))
                {
                    IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                    IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                    newAdTreeElementCopy.Right = adTreeCopy;

                    IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                    adTrees.Add(workInProgress);
                }

                // Try to attach the new element via the bridging adposition.
                foreach (IPattern pattern in myPatterns)
                {
                    // If the pattern can be a technical (epsilon) adposition.
                    if (pattern.MorphemeRule.Equals(MorphemeRule.Epsilon) &&
                        !pattern.LeftRule.Equals(PatternRule.Nothing) && !pattern.RightRule.Equals(PatternRule.Nothing))
                    {
                        IAdTree bridgingAdposition = new AdTree(new Morpheme(""), pattern);

                        bool canAttachAdTreeToLeft = bridgingAdposition.CanAttachToLeft(adTree);
                        bool canAttachAdTreeToRight = bridgingAdposition.CanAttachToRight(adTree);


                        // If the adtree can be attached to the left and the new element to the right.
                        if (canAttachAdTreeToLeft && bridgingAdposition.CanAttachToRight(newAdTreeElement))
                        {
                            IAdTree bridgingAdPositionCopy = GetCopyOnSamePath(bridgingAdposition);
                            IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                            IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);

                            bridgingAdPositionCopy.Right = newAdTreeElementCopy;
                            bridgingAdPositionCopy.Left = adTreeCopy;

                            IAdTree workInProgress = GetWorkInProgressAdTree(bridgingAdPositionCopy);
                            adTrees.Add(workInProgress);
                        }

                        // If the adtree can be attached to the right and the new element to the left.
                        if (canAttachAdTreeToRight && bridgingAdposition.CanAttachToLeft(newAdTreeElement))
                        {
                            IAdTree bridgingAdPositionCopy = GetCopyOnSamePath(bridgingAdposition);
                            IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                            IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);

                            bridgingAdPositionCopy.Right = adTreeCopy;
                            bridgingAdPositionCopy.Left = newAdTreeElementCopy;

                            IAdTree workInProgress = GetWorkInProgressAdTree(bridgingAdPositionCopy);
                            adTrees.Add(workInProgress);
                        }

                        // If one more between bridge could be used.
                        if (canAttachAdTreeToLeft || canAttachAdTreeToRight)
                        {
                            foreach (IPattern pattern2 in myPatterns)
                            {
                                // If the pattern can be a technical (epsilon) adposition.
                                if (pattern.MorphemeRule.Equals(MorphemeRule.Epsilon) &&
                                    !pattern2.LeftRule.Equals(PatternRule.Nothing) && !pattern2.RightRule.Equals(PatternRule.Nothing))
                                {
                                    IAdTree betweenBridgingAdposition = new AdTree(new Morpheme(""), pattern2);

                                    // If the adtree can be attached to the left and a between bridge can be attach to the right and
                                    // the new element to the left of the between bridge.
                                    if (bridgingAdposition.CanAttachToLeft(adTree) && bridgingAdposition.CanAttachToRight(betweenBridgingAdposition) &&
                                        betweenBridgingAdposition.CanAttachToLeft(newAdTreeElement))
                                    {
                                        IAdTree bridgingAdPositionCopy = GetCopyOnSamePath(bridgingAdposition);
                                        IAdTree betweenbridgingAdPositionCopy = GetCopyOnSamePath(betweenBridgingAdposition);
                                        IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                                        IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);

                                        // Note: we do not need to make a copy until there is multiple assignment possibilities for pattern2.
                                        bridgingAdPositionCopy.Right = betweenbridgingAdPositionCopy;
                                        bridgingAdPositionCopy.Left = adTreeCopy;

                                        betweenbridgingAdPositionCopy.Left = newAdTreeElementCopy;

                                        IAdTree workInProgress = GetWorkInProgressAdTree(betweenbridgingAdPositionCopy);
                                        adTrees.Add(workInProgress);
                                    }


                                    // If the adtree can be attached to the right and a between bridge can be attach to the left and
                                    // the new element to the left of the between bridge.
                                    if (bridgingAdposition.CanAttachToRight(adTree) && bridgingAdposition.CanAttachToLeft(betweenBridgingAdposition) &&
                                        betweenBridgingAdposition.CanAttachToLeft(newAdTreeElement))
                                    {
                                        IAdTree bridgingAdPositionCopy = GetCopyOnSamePath(bridgingAdposition);
                                        IAdTree betweenbridgingAdPositionCopy = GetCopyOnSamePath(betweenBridgingAdposition);
                                        IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                                        IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);

                                        // Note: we do not need to make a copy until there is multiple assignment possibilities for pattern2.
                                        bridgingAdPositionCopy.Right = adTreeCopy;
                                        bridgingAdPositionCopy.Left = betweenbridgingAdPositionCopy;

                                        betweenbridgingAdPositionCopy.Left = newAdTreeElementCopy;

                                        IAdTree workInProgress = GetWorkInProgressAdTree(betweenbridgingAdPositionCopy);
                                        adTrees.Add(workInProgress);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        private IAdTree GetWorkInProgressAdTree(IAdTree currentWorkInProgressAdTree)
        {
            IEnumerable<IAdTree> adTrees = new IAdTree[] { currentWorkInProgressAdTree }.Concat(currentWorkInProgressAdTree.AdPositions);
            foreach (IAdTree adTree in adTrees)
            {
                if (adTree.Left == null && !adTree.Pattern.LeftRule.Equals(PatternRule.Nothing) ||
                    adTree.Right == null && !adTree.Pattern.RightRule.Equals(PatternRule.Nothing))
                {
                    return adTree;
                }
            }

            return currentWorkInProgressAdTree.Root;
        }


        private IAdTree GetCopyOnSamePath(IAdTree adTreeToCopy)
        {
            byte[] path = adTreeToCopy.GetPath();
            IAdTree root = adTreeToCopy.Root;
            IAdTree copy = root.MakeShallowCopy();
            if (copy.TryGetAdTree(path, out IAdTree result))
            {
                return result;
            }

            throw new InvalidOperationException("Failed to properly copy the adtree.");
        }
    }
}
