using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using Krino.Vertical.Utils.Graphs;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class AdTreeBuilder
    {
        private DirectedGraph<GrammarCharacter, IPattern> myPatternGraph;
        private List<IPattern> myMorphemePatterns;

        private List<IAdTree> myActiveAdTrees = new List<IAdTree>();

        public AdTreeBuilder(IEnumerable<IPattern> patterns)
        {
            myMorphemePatterns = patterns.Where(x => !x.MorphemeRule.Equals(MorphemeRule.Epsilon)).ToList();

            GrammarCharacter[] grammarCharacters = Enum.GetValues(typeof(GrammarCharacter)).Cast<GrammarCharacter>().ToArray();

            myPatternGraph = new DirectedGraph<GrammarCharacter, IPattern>();
            foreach (GrammarCharacter grammarCharacter in grammarCharacters)
            {
                myPatternGraph.AddVertex(grammarCharacter.ToString(), grammarCharacter);
            }

            foreach (IPattern pattern in patterns)
            {
                // If it is an adposition related rule.
                if (!pattern.LeftRule.Equals(PatternRule.Nothing) && !pattern.RightRule.Equals(PatternRule.Nothing))
                {
                    foreach (GrammarCharacter leftGrammarCharacter in grammarCharacters)
                    {
                        if (pattern.LeftRule.IsMatch(leftGrammarCharacter))
                        {
                            foreach (GrammarCharacter rightGrammarCharacter in grammarCharacters)
                            {
                                if (pattern.RightRule.IsMatch(rightGrammarCharacter))
                                {
                                    myPatternGraph.AddEdge(leftGrammarCharacter.ToString(), rightGrammarCharacter.ToString(), pattern);
                                    myPatternGraph.AddEdge(rightGrammarCharacter.ToString(), leftGrammarCharacter.ToString(), pattern);
                                }
                            }
                        }
                    }
                }
            }
        }

        public IReadOnlyList<IAdTree> ActiveAdTrees => myActiveAdTrees.Select(x => x.Root).ToList();

        public bool AddMorpheme(IMorpheme morpheme)
        {
            bool isAdded = false;

            IEnumerable<IPattern> morphemeMatchingPatterns = myMorphemePatterns.Where(x => x.MorphemeRule.IsMatch(morpheme.Morph, morpheme.Attributes));

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
                        // Create the adtree element from the incoming morpheme and its pattern.
                        IAdTree newAdTreeElement = new AdTree(morpheme, pattern);

                        // Try to attach the morpheme element to the adtree.
                        TryToAttachAdTreeElement(currentAdTree, newAdTreeElement, newListOfAdTrees);
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
                    AdTree newAdTree = new AdTree(morpheme, pattern);
                    myActiveAdTrees.Add(newAdTree);
                }

                if (myActiveAdTrees.Count > 0)
                {
                    isAdded = true;
                }
            }


            return isAdded;
        }

        private void TryToAttachAdTreeElement(IAdTree adTree, IAdTree newAdTreeElement, List<IAdTree> adTrees)
        {
            bool isAttached = false;

            // Try to use the adtree as the adposition and attach the new element to its left.
            if (adTree.Left == null && adTree.CanAttachToLeft(newAdTreeElement))
            {
                IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                adTreeCopy.Left = newAdTreeElementCopy;

                IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                adTrees.Add(workInProgress);

                isAttached = true;
            }
            // Try to use the adtree as the adposition and attach the new element to its right.
            else if (adTree.Right == null && adTree.CanAttachToRight(newAdTreeElement))
            {
                IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                adTreeCopy.Right = newAdTreeElementCopy;

                IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                adTrees.Add(workInProgress);

                isAttached = true;
            }
            // If the adtree adposition is free to connect.
            else if (adTree.AdPosition == null)
            {
                // Try to use the new element as the adposition and attach the adtree to its left.
                if (newAdTreeElement.CanAttachToLeft(adTree))
                {
                    IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                    IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                    newAdTreeElementCopy.Left = adTreeCopy;

                    IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                    adTrees.Add(workInProgress);

                    isAttached = true;
                }
                // Try to use the new element as the adposition and attach the adtree to its right.
                else if (newAdTreeElement.CanAttachToRight(adTree))
                {
                    IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                    IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                    newAdTreeElementCopy.Right = adTreeCopy;

                    IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                    adTrees.Add(workInProgress);

                    isAttached = true;
                }
            }

            // If it is not possible to directly attach the element then try to attach it via bridging 'epsilon' adtrees.
            if (!isAttached)
            {
                //// left: 1; adposition: 2; right: 3
                //int attachPosition = 0;
                //if (adTree.Left != null && adTree.Right != null && adTree.AdPosition == null)
                //{
                //    // AdPosition
                //    attachPosition = 2;
                //}
                //else if (adTree.Left == null)
                //{
                //    attachPosition = 1;
                //}
                //else if (adTree.Right == null)
                //{
                //    attachPosition = 3;
                //}
                //else
                //{
                //    // Note: the position for attaching was not identified.
                //    return;
                //}


                GrammarCharacter adTreeGrammarCharacter = adTree.GrammarCharacter != GrammarCharacter.Epsilon ? adTree.GrammarCharacter : adTree.InheritedGrammarCharacter;
                //if (attachPosition == 2)
                //{
                //    adTreeGrammarCharacter = adTree.InheritedGrammarCharacter;
                //}
                //else if (attachPosition == )
                //{
                //    adTreeGrammarCharacter = adTree.Left.InheritedGrammarCharacter;
                //}



                GrammarCharacter newAdTreeElementGrammarCharacter = newAdTreeElement.GrammarCharacter;

                // Get possibilities how to connect new element.
                IEnumerable<IReadOnlyList<DirectedEdge<IPattern>>> connectionPaths = myPatternGraph
                    .FindAllPaths(adTreeGrammarCharacter.ToString(), newAdTreeElementGrammarCharacter.ToString());

                foreach (IReadOnlyList<DirectedEdge<IPattern>> path in connectionPaths)
                {
                    // If the path contains only epsilon adpositions.
                    if (path.All(x => x.Value.MorphemeRule.IsMatch(GrammarCharacter.Epsilon)))
                    {
                        IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                        IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);

                        IAdTree previousBridgingAdTree = null;

                        // Go via the path.
                        for (int i = 0; i < path.Count; ++i)
                        {
                            DirectedEdge<IPattern> edge = path[i];

                            IAdTree bridgingAdTree = new AdTree(new Morpheme(""), edge.Value);

                            // If it is the first item on the path.
                            if (i == 0)
                            {
                                if (bridgingAdTree.CanAttachToLeft(adTree))
                                {
                                    bridgingAdTree.Left = adTreeCopy;
                                }
                                else if (bridgingAdTree.CanAttachToRight(adTree))
                                {
                                    bridgingAdTree.Right = adTreeCopy;
                                }
                                else
                                {
                                    // Note: this should never happen but do not throw exception.
                                    //       It is not worth to interrupt krino.
                                    break;
                                }
                            }
                            else
                            {
                                if (previousBridgingAdTree.Left == null && previousBridgingAdTree.CanAttachToLeft(bridgingAdTree))
                                {
                                    previousBridgingAdTree.Left = bridgingAdTree;
                                }
                                else if (previousBridgingAdTree.Right == null && previousBridgingAdTree.CanAttachToRight(bridgingAdTree))
                                {
                                    previousBridgingAdTree.Right = bridgingAdTree;
                                }
                                else
                                {
                                    // Note: this should never happen but do not throw exception.
                                    //       It is not worth to interrupt krino.
                                    break;
                                }
                            }

                            // If it is the last item on the path.
                            if (i == path.Count - 1)
                            {
                                if (bridgingAdTree.CanAttachToRight(newAdTreeElement))
                                {
                                    bridgingAdTree.Right = newAdTreeElementCopy;

                                    IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                                    adTrees.Add(workInProgress);
                                }
                                else if (bridgingAdTree.CanAttachToLeft(newAdTreeElement))
                                {
                                    bridgingAdTree.Left = newAdTreeElementCopy;

                                    IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                                    adTrees.Add(workInProgress);
                                }
                                else
                                {
                                    // Note: this should never happen but do not throw exception.
                                    //       It is not worth to interrupt krino.
                                    break;
                                }
                            }

                            previousBridgingAdTree = bridgingAdTree;
                        }

                    }
                }
            }
        }


        //private void AttachAdTreeElement(IAdTree adTree, IAdTree newAdTreeElement, List<IAdTree> adTrees)
        //{
        //    // Try to use the adtree as the adposition and attach the new element to its left.
        //    if (adTree.Left == null && adTree.CanAttachToLeft(newAdTreeElement))
        //    {
        //        IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
        //        IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
        //        adTreeCopy.Left = newAdTreeElementCopy;

        //        IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
        //        adTrees.Add(workInProgress);
        //    }

        //    // Try to use the adtree as the adposition and attach the new element to its right.
        //    if (adTree.Right == null && adTree.CanAttachToRight(newAdTreeElement))
        //    {
        //        IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
        //        IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
        //        adTreeCopy.Right = newAdTreeElementCopy;

        //        IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
        //        adTrees.Add(workInProgress);
        //    }


        //    // If the adtree adposition is still free.
        //    if (adTree.AdPosition == null)
        //    {
        //        // Try to use the new element as the adposition and attach the adtree to its left.
        //        if (newAdTreeElement.CanAttachToLeft(adTree))
        //        {
        //            IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
        //            IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
        //            newAdTreeElementCopy.Left = adTreeCopy;

        //            IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
        //            adTrees.Add(workInProgress);
        //        }

        //        // Try to use the new element as the adposition and attach the adtree to its right.
        //        if (newAdTreeElement.CanAttachToRight(adTree))
        //        {
        //            IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
        //            IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
        //            newAdTreeElementCopy.Right = adTreeCopy;

        //            IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
        //            adTrees.Add(workInProgress);
        //        }

        //        // Try to attach the new element via the bridging adposition.
        //        foreach (IPattern pattern in myPatterns)
        //        {
        //            // If the pattern can be a technical (epsilon) adposition.
        //            if (pattern.MorphemeRule.Equals(MorphemeRule.Epsilon) &&
        //                !pattern.LeftRule.Equals(PatternRule.Nothing) && !pattern.RightRule.Equals(PatternRule.Nothing))
        //            {
        //                IAdTree bridgingAdposition = new AdTree(new Morpheme(""), pattern);

        //                bool canAttachAdTreeToLeft = bridgingAdposition.CanAttachToLeft(adTree);
        //                bool canAttachAdTreeToRight = bridgingAdposition.CanAttachToRight(adTree);


        //                // If the adtree can be attached to the left and the new element to the right.
        //                if (canAttachAdTreeToLeft && bridgingAdposition.CanAttachToRight(newAdTreeElement))
        //                {
        //                    IAdTree bridgingAdPositionCopy = GetCopyOnSamePath(bridgingAdposition);
        //                    IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
        //                    IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);

        //                    bridgingAdPositionCopy.Right = newAdTreeElementCopy;
        //                    bridgingAdPositionCopy.Left = adTreeCopy;

        //                    IAdTree workInProgress = GetWorkInProgressAdTree(bridgingAdPositionCopy);
        //                    adTrees.Add(workInProgress);
        //                }

        //                // If the adtree can be attached to the right and the new element to the left.
        //                if (canAttachAdTreeToRight && bridgingAdposition.CanAttachToLeft(newAdTreeElement))
        //                {
        //                    IAdTree bridgingAdPositionCopy = GetCopyOnSamePath(bridgingAdposition);
        //                    IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
        //                    IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);

        //                    bridgingAdPositionCopy.Right = adTreeCopy;
        //                    bridgingAdPositionCopy.Left = newAdTreeElementCopy;

        //                    IAdTree workInProgress = GetWorkInProgressAdTree(bridgingAdPositionCopy);
        //                    adTrees.Add(workInProgress);
        //                }

        //                // If one more between bridge could be used.
        //                if (canAttachAdTreeToLeft || canAttachAdTreeToRight)
        //                {
        //                    foreach (IPattern pattern2 in myPatterns)
        //                    {
        //                        // If the pattern can be a technical (epsilon) adposition.
        //                        if (pattern.MorphemeRule.Equals(MorphemeRule.Epsilon) &&
        //                            !pattern2.LeftRule.Equals(PatternRule.Nothing) && !pattern2.RightRule.Equals(PatternRule.Nothing))
        //                        {
        //                            IAdTree betweenBridgingAdposition = new AdTree(new Morpheme(""), pattern2);

        //                            // If the adtree can be attached to the left and a between bridge can be attach to the right and
        //                            // the new element to the left of the between bridge.
        //                            if (bridgingAdposition.CanAttachToLeft(adTree) && bridgingAdposition.CanAttachToRight(betweenBridgingAdposition) &&
        //                                betweenBridgingAdposition.CanAttachToLeft(newAdTreeElement))
        //                            {
        //                                IAdTree bridgingAdPositionCopy = GetCopyOnSamePath(bridgingAdposition);
        //                                IAdTree betweenbridgingAdPositionCopy = GetCopyOnSamePath(betweenBridgingAdposition);
        //                                IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
        //                                IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);

        //                                // Note: we do not need to make a copy until there is multiple assignment possibilities for pattern2.
        //                                bridgingAdPositionCopy.Right = betweenbridgingAdPositionCopy;
        //                                bridgingAdPositionCopy.Left = adTreeCopy;

        //                                betweenbridgingAdPositionCopy.Left = newAdTreeElementCopy;

        //                                IAdTree workInProgress = GetWorkInProgressAdTree(betweenbridgingAdPositionCopy);
        //                                adTrees.Add(workInProgress);
        //                            }


        //                            // If the adtree can be attached to the right and a between bridge can be attach to the left and
        //                            // the new element to the left of the between bridge.
        //                            if (bridgingAdposition.CanAttachToRight(adTree) && bridgingAdposition.CanAttachToLeft(betweenBridgingAdposition) &&
        //                                betweenBridgingAdposition.CanAttachToLeft(newAdTreeElement))
        //                            {
        //                                IAdTree bridgingAdPositionCopy = GetCopyOnSamePath(bridgingAdposition);
        //                                IAdTree betweenbridgingAdPositionCopy = GetCopyOnSamePath(betweenBridgingAdposition);
        //                                IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
        //                                IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);

        //                                // Note: we do not need to make a copy until there is multiple assignment possibilities for pattern2.
        //                                bridgingAdPositionCopy.Right = adTreeCopy;
        //                                bridgingAdPositionCopy.Left = betweenbridgingAdPositionCopy;

        //                                betweenbridgingAdPositionCopy.Left = newAdTreeElementCopy;

        //                                IAdTree workInProgress = GetWorkInProgressAdTree(betweenbridgingAdPositionCopy);
        //                                adTrees.Add(workInProgress);
        //                            }
        //                        }
        //                    }
        //                }
        //            }
        //        }
        //    }
        //}

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
