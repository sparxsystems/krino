using Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions;
using Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules;
using Krino.Domain.ConstructiveAdpositionalGrammar.ConstructiveDictionaries;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Graphs;
using Krino.Vertical.Utils.Rules;
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

        private IConstructiveDictionary myConstructiveDictionary;

        private List<IAdTree> myActiveAdTrees = new List<IAdTree>();

        public AdTreeBuilder(IConstructiveDictionary constructiveDictionary)
        {
            myConstructiveDictionary = constructiveDictionary;
        }

        public IReadOnlyList<IAdTree> ActiveAdTrees => myActiveAdTrees.Select(x => x.Root).ToList();

        public bool AddWord(string value, int maxMorphDistance = 0)
        {
            List<IAdTree> adTreesToAdd = new List<IAdTree>();

            IEnumerable<IReadOnlyList<IMorpheme>> morphemeSequences = myConstructiveDictionary.FindMorphemeSequences(value, maxMorphDistance);

            // Go via sequences.
            foreach (IReadOnlyList<IMorpheme> sequence in morphemeSequences)
            {
                bool isCancelled = false;
                IMorpheme lexeme = null;
                AdTreeBuilder localAdTreeBuilder = new AdTreeBuilder(myConstructiveDictionary);
                foreach (IMorpheme morpheme in sequence)
                {
                    if (morpheme.IsLexeme)
                    {
                        if (lexeme != null)
                        {
                            if (!localAdTreeBuilder.AddMorpheme(lexeme))
                            {
                                isCancelled = true;
                                break;
                            }
                        }

                        lexeme = morpheme;
                    }
                    else
                    {
                        if (!localAdTreeBuilder.AddMorpheme(morpheme))
                        {
                            isCancelled = true;
                            break;
                        }
                    }
                }

                if (!isCancelled)
                {
                    if (lexeme != null)
                    {
                        if (!localAdTreeBuilder.AddMorpheme(lexeme))
                        {
                            isCancelled = true;
                        }
                    }

                    if (!isCancelled)
                    {
                        adTreesToAdd.AddRange(localAdTreeBuilder.ActiveAdTrees);
                    }
                }
            }

            bool result = AddHomonyms(adTreesToAdd);
            return result;
        }

        public bool AddMorpheme(IMorpheme lexeme)
        {
            bool result = AddHomonyms(new IMorpheme[] { lexeme });
            return result;
        }

        public bool AddHomonyms(IEnumerable<IMorpheme> homonymMorphemes)
        {
            IEnumerable<IAdTree> homonymAdTrees = GetAdTrees(homonymMorphemes);
            bool isAdded = AddHomonyms(homonymAdTrees);
            return isAdded;
        }

        public bool AddHomonyms(IEnumerable<IAdTree> homonymAdTrees)
        {
            bool isAdded = false;

            // Update active adtrees.
            if (myActiveAdTrees.Count != 0)
            {
                List<IAdTree> newListOfAdTrees = new List<IAdTree>();

                foreach (IAdTree activeAdTree in myActiveAdTrees)
                {
                    foreach (IAdTree newAdTree in homonymAdTrees)
                    {
                        TryToAttachAdTree(activeAdTree, newAdTree, newListOfAdTrees);
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
                myActiveAdTrees.AddRange(homonymAdTrees);

                if (myActiveAdTrees.Count > 0)
                {
                    isAdded = true;
                }
            }

            return isAdded;
        }


        private IEnumerable<IAdTree> GetAdTrees(IEnumerable<IMorpheme> morphemes)
        {
            foreach (IMorpheme morpheme in morphemes)
            {
                IEnumerable<IPattern> matchingPatterns = myConstructiveDictionary.FindMatchingPatterns(morpheme);

                // Go via patterns matching the incoming morpheme.
                foreach (IPattern pattern in matchingPatterns)
                {
                    // Create the adtree element from the incoming morpheme and its pattern.
                    IAdTree newAdTree = new AdTree(morpheme, pattern);

                    yield return newAdTree;
                }
            }
        }

        private void TryToAttachAdTree(IAdTree adTree, IAdTree newAdTreeElement, List<IAdTree> results)
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
                if (newAdTreeElement.Left == null)
                {
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
                }

                // Try to attach the adtree directly via new element's right.
                if (newAdTreeElement.Right == null)
                {
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
                }

                // Also try to connect the adtree and the new element indirectly via adtree adposition.
                TryToAttachIndirectly(EAttachPosition.TopLeft, adTree, newAdTreeElement, results);
                TryToAttachIndirectly(EAttachPosition.TopRight, adTree, newAdTreeElement, results);
            }
        }

        private void TryToAttachIndirectly(EAttachPosition initialAttachPosition, IAdTree start, IAdTree end, List<IAdTree> results)
        {
            GrammarCharacter startGrammarCharacter = GrammarCharacter.Epsilon;
            GrammarCharacter endGrammarCharacter = GrammarCharacter.Epsilon;

            // If start adTree connects the bridge via its left.
            if (initialAttachPosition == EAttachPosition.BottomLeft)
            {
                startGrammarCharacter = start.Pattern.LeftRule.MorphemeRule.GrammarCharacter;
                endGrammarCharacter = ChooseGrammarCharacter(end.GrammarCharacter, end.Pattern.RightRule.MorphemeRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If start adTree connects the bridge via its right.
            else if (initialAttachPosition == EAttachPosition.BottomRight)
            {
                startGrammarCharacter = start.Pattern.RightRule.MorphemeRule.GrammarCharacter;
                endGrammarCharacter = ChooseGrammarCharacter(end.GrammarCharacter, end.Pattern.RightRule.MorphemeRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If the bridge connects the start adtree via its left.
            else if (initialAttachPosition == EAttachPosition.TopLeft)
            {
                startGrammarCharacter = ChooseGrammarCharacter(start.GrammarCharacter, start.Pattern.RightRule.MorphemeRule.GrammarCharacter, start.InheritedGrammarCharacter);
                endGrammarCharacter = ChooseGrammarCharacter(end.GrammarCharacter, end.Pattern.RightRule.MorphemeRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }
            // If the bridge connects the start adtree via its right.
            else if (initialAttachPosition == EAttachPosition.TopRight)
            {
                startGrammarCharacter = ChooseGrammarCharacter(start.GrammarCharacter, start.Pattern.RightRule.MorphemeRule.GrammarCharacter, start.InheritedGrammarCharacter);
                endGrammarCharacter = ChooseGrammarCharacter(end.GrammarCharacter, end.Pattern.RightRule.MorphemeRule.GrammarCharacter, end.InheritedGrammarCharacter);
            }

            // Get possibile ways how to connect new element.
            IEnumerable<IReadOnlyList<DirectedEdge<IPattern>>> connectionPaths = myConstructiveDictionary.PatternGraph
                .FindAllPaths(startGrammarCharacter.ToString(), endGrammarCharacter.ToString());

            // Go via all possible ways.
            foreach (IReadOnlyList<DirectedEdge<IPattern>> path in connectionPaths)
            {
                // If all elements is possible to create without morphs.
                if (path.All(x => x.Value.MorphemeRule.MorphRule.Equals(Rule.Nothing<string>()) ||
                                  x.Value.MorphemeRule.MorphRule.Evaluate("")))
                {
                    IAdTree startCopy = GetCopyOnSamePath(start);
                    IAdTree endCopy = GetCopyOnSamePath(end);

                    IAdTree previousBridge = null;

                    // Go via the path.
                    for (int i = 0; i < path.Count; ++i)
                    {
                        DirectedEdge<IPattern> edge = path[i];

                        IAdTree bridge = new AdTree(new Morpheme("") { Attributes = edge.Value.MorphemeRule.GrammarCharacter.GetAttributes() }, edge.Value);

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

        private GrammarCharacter ChooseGrammarCharacter(params GrammarCharacter[] grammarCharacters)
        {
            GrammarCharacter result = grammarCharacters.FirstOrDefault(x => x != GrammarCharacter.Epsilon);
            return result;
        }
    }
}
