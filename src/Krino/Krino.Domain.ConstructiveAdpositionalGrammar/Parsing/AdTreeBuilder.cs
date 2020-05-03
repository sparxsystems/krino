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
        private enum EAttachPosition
        {
            /// <summary>
            /// The element attached as the left child.
            /// </summary>
            TopLeft,

            /// <summary>
            /// The element attached as the right child.
            /// </summary>
            TopRight,

            /// <summary>
            /// The elemet is the adposition (parent) attaching to the left.
            /// </summary>
            BottomLeft,

            /// <summary>
            /// The elemet is the adposition (parent) attaching to the right.
            /// </summary>
            BottomRight,
        };

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

        private void TryToAttachAdTreeElement(IAdTree adTree, IAdTree newAdTreeElement, List<IAdTree> results)
        {
            // If the adtree can attach something to left.
            if (adTree.Left == null && !adTree.Pattern.LeftRule.Equals(PatternRule.Nothing))
            {
                // Try to attach the new element directly.
                if (adTree.CanAttachToLeft(newAdTreeElement))
                {
                    IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                    IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                    adTreeCopy.Left = newAdTreeElementCopy;

                    IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                    results.Add(workInProgress);
                }
                // Try to attach the new element indirectly.
                else
                {
                    TryToAttachIndirectly(EAttachPosition.BottomLeft, adTree, newAdTreeElement, results);
                }
            }

            // If adtree can attach something to right.
            if (adTree.Right == null && !adTree.Pattern.RightRule.Equals(PatternRule.Nothing))
            {
                // Try to attach the new element directly.
                if (adTree.CanAttachToRight(newAdTreeElement))
                {
                    IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                    IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                    adTreeCopy.Right = newAdTreeElementCopy;

                    IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                    results.Add(workInProgress);
                }
                // Try to attach the new element indirectly.
                else
                {
                    TryToAttachIndirectly(EAttachPosition.BottomRight, adTree, newAdTreeElement, results);
                }
            }

            // If the adtree has free adposition - i.e. it can be attached to left or right.
            if (adTree.AdPosition == null)
            {
                // Try to attach the adtree directly via new element's left.
                if (newAdTreeElement.CanAttachToLeft(adTree))
                {
                    IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                    IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                    newAdTreeElementCopy.Left = adTreeCopy;

                    IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                    results.Add(workInProgress);
                }
                // Try to attach the adtree indirectly via new element's left.
                else
                {
                    TryToAttachIndirectly(EAttachPosition.BottomLeft, newAdTreeElement, adTree, results);
                }

                // Try to attach the adtree directly via new element's right.
                if (newAdTreeElement.CanAttachToRight(adTree))
                {
                    IAdTree newAdTreeElementCopy = GetCopyOnSamePath(newAdTreeElement);
                    IAdTree adTreeCopy = GetCopyOnSamePath(adTree);
                    newAdTreeElementCopy.Right = adTreeCopy;

                    IAdTree workInProgress = GetWorkInProgressAdTree(newAdTreeElementCopy);
                    results.Add(workInProgress);
                }
                // Try to attach the adtree indirectly via new element's right.
                else
                {
                    TryToAttachIndirectly(EAttachPosition.BottomRight, newAdTreeElement, adTree, results);
                }

                // Also try to connect the adtree and the new element indirectly via adtree adposition.
                TryToAttachIndirectly(EAttachPosition.TopLeft, adTree, newAdTreeElement, results);
                TryToAttachIndirectly(EAttachPosition.TopRight, adTree, newAdTreeElement, results);
            }
        }

        private void TryToAttachIndirectly(EAttachPosition initialAttachPosition, IAdTree start, IAdTree end, List<IAdTree> results)
        {
            IEnumerable<GrammarCharacter> startGrammarCharacters = Enumerable.Empty<GrammarCharacter>();
            IEnumerable<GrammarCharacter> endGrammarCharacters = Enumerable.Empty<GrammarCharacter>();

            // If start adTree connects the bridge via its left.
            if (initialAttachPosition == EAttachPosition.BottomLeft)
            {
                startGrammarCharacters = start.Pattern.LeftRule.GetMatchingGrammarCharacters().Where(x => x != GrammarCharacter.Epsilon);
                endGrammarCharacters = new GrammarCharacter[] { end.GrammarCharacter, end.InheritedGrammarCharacter }
                    .Concat(end.Pattern.RightRule.GetMatchingGrammarCharacters())
                    .Where(x => x != GrammarCharacter.Epsilon)
                    .Distinct();
            }
            // If start adTree connects the bridge via its right.
            else if (initialAttachPosition == EAttachPosition.BottomRight)
            {
                startGrammarCharacters = start.Pattern.RightRule.GetMatchingGrammarCharacters().Where(x => x != GrammarCharacter.Epsilon);
                endGrammarCharacters = new GrammarCharacter[] { end.GrammarCharacter, end.InheritedGrammarCharacter }
                    .Concat(end.Pattern.RightRule.GetMatchingGrammarCharacters())
                    .Where(x => x != GrammarCharacter.Epsilon)
                    .Distinct();
            }
            // If the bridge connects the start adtree via its left.
            else if (initialAttachPosition == EAttachPosition.TopLeft)
            {
                startGrammarCharacters = new GrammarCharacter[] { start.GrammarCharacter, start.InheritedGrammarCharacter }
                    .Concat(start.Pattern.RightRule.GetMatchingGrammarCharacters())
                    .Where(x => x != GrammarCharacter.Epsilon)
                    .Distinct();
                endGrammarCharacters = new GrammarCharacter[] { end.GrammarCharacter, end.InheritedGrammarCharacter }
                    .Concat(end.Pattern.RightRule.GetMatchingGrammarCharacters())
                    .Where(x => x != GrammarCharacter.Epsilon)
                    .Distinct();
            }
            // If the bridge connects the start adtree via its right.
            else if (initialAttachPosition == EAttachPosition.TopRight)
            {
                startGrammarCharacters = new GrammarCharacter[] { start.GrammarCharacter, start.InheritedGrammarCharacter }
                    .Concat(start.Pattern.RightRule.GetMatchingGrammarCharacters())
                    .Where(x => x != GrammarCharacter.Epsilon)
                    .Distinct();
                endGrammarCharacters = new GrammarCharacter[] { end.GrammarCharacter, end.InheritedGrammarCharacter }
                    .Concat(end.Pattern.RightRule.GetMatchingGrammarCharacters())
                    .Where(x => x != GrammarCharacter.Epsilon)
                    .Distinct();
            }

            foreach (GrammarCharacter startGrammarCharacter in startGrammarCharacters)
            {
                foreach (GrammarCharacter endGrammarCharacter in endGrammarCharacters)
                {
                    // Get possibile ways how to connect new element.
                    IEnumerable<IReadOnlyList<DirectedEdge<IPattern>>> connectionPaths = myPatternGraph
                        .FindAllPaths(startGrammarCharacter.ToString(), endGrammarCharacter.ToString());

                    // Go via all possible ways.
                    foreach (IReadOnlyList<DirectedEdge<IPattern>> path in connectionPaths)
                    {
                        // The path is usable only if all its parts have epsilon grammar character.
                        if (path.All(x => x.Value.MorphemeRule.IsMatch(GrammarCharacter.Epsilon)))
                        {
                            IAdTree startCopy = GetCopyOnSamePath(start);
                            IAdTree endCopy = GetCopyOnSamePath(end);

                            IAdTree previousBridge = null;

                            // Go via the path.
                            for (int i = 0; i < path.Count; ++i)
                            {
                                DirectedEdge<IPattern> edge = path[i];

                                IAdTree bridge = new AdTree(new Morpheme(""), edge.Value);

                                // If it is the first item on the path.
                                if (i == 0)
                                {
                                    if (initialAttachPosition == EAttachPosition.BottomLeft)
                                    {
                                        if (startCopy.Left == null && startCopy.CanAttachToLeft(bridge))
                                        {
                                            startCopy.Left = bridge;
                                        }
                                        else
                                        {
                                            break;
                                        }
                                    }
                                    else if (initialAttachPosition == EAttachPosition.BottomRight)
                                    {
                                        if (startCopy.Right == null && startCopy.CanAttachToRight(bridge))
                                        {
                                            startCopy.Right = bridge;
                                        }
                                        else
                                        {
                                            break;
                                        }
                                    }
                                    else if (initialAttachPosition == EAttachPosition.TopLeft)
                                    {
                                        if (bridge.Left == null && bridge.CanAttachToLeft(startCopy))
                                        {
                                            bridge.Left = startCopy;
                                        }
                                        else
                                        {
                                            break;
                                        }
                                    }
                                    else if (initialAttachPosition == EAttachPosition.TopRight)
                                    {
                                        if (bridge.Right == null && bridge.CanAttachToRight(startCopy))
                                        {
                                            bridge.Right = startCopy;
                                        }
                                        else
                                        {
                                            break;
                                        }
                                    }
                                }
                                else
                                {
                                    if (previousBridge.Left == null && previousBridge.CanAttachToLeft(bridge))
                                    {
                                        previousBridge.Left = bridge;
                                    }
                                    else if (previousBridge.Right == null && previousBridge.CanAttachToRight(bridge))
                                    {
                                        previousBridge.Right = bridge;
                                    }
                                    else
                                    {
                                        // Note: this should never happen but do not throw exception.
                                        //       It is not worth to interrupt krino.
                                        break;
                                    }
                                }

                                // If it is the last item in the path.
                                if (i == path.Count - 1)
                                {
                                    if (bridge.Left == null && bridge.CanAttachToLeft(end))
                                    {
                                        bridge.Left = endCopy;

                                        IAdTree workInProgress = GetWorkInProgressAdTree(endCopy);
                                        results.Add(workInProgress);
                                    }
                                    else if (bridge.Right == null && bridge.CanAttachToRight(end))
                                    {
                                        bridge.Right = endCopy;

                                        IAdTree workInProgress = GetWorkInProgressAdTree(endCopy);
                                        results.Add(workInProgress);
                                    }
                                    else
                                    {
                                        // Note: this should never happen but do not throw exception.
                                        //       It is not worth to interrupt krino.
                                        break;
                                    }
                                }

                                previousBridge = bridge;
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
